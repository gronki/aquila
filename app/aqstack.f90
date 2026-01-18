program aqstack

  !----------------------------------------------------------------------------!

  use globals
  use framehandling
  use statistics
  use stacking
  use hotpixels
  use findstar
  use str_utils_m
  use iso_fortran_env, only: real64, stderr => error_unit

  !----------------------------------------------------------------------------!

  implicit none

  ! allowed values: average, median, sigclip
  character(len = 32) :: method = "average", strategy = "", align_method = "affine"
  character(len = 256) :: output_fn = "", ref_fn = "", &
        bias_fn = "", dark_fn = "", flat_fn = ""
  character(len = 64) :: output_suff = ""
  character(len = 256), allocatable :: input_fn(:)
  logical :: cfg_align_frames = .false.
  logical :: cfg_process_only = .false.
  logical :: cfg_normalize = .false.
  logical :: cfg_correct_hot = .true., cfg_correct_hot_only = .false.
  logical :: cfg_dark_optimize = .false.
  logical :: cfg_dark_is_dirty = .false.
  logical :: cfg_resampling = .false.
  logical :: cfg_temperature_filter = .false.
  logical :: cfg_prealign_polygon = .false.
  logical :: cfg_is_cfa = .false.
  real(r64_k) :: resample_factor = 2
  real(buf_k) :: cfg_temperature_point = 0, cfg_temperature_tolerance = 0.5
  real(buf_k) :: hotpixel_threshold_sigma = 5.0, darkopt_sigma = 0.0
  integer :: margin = 10
  real(real64) :: t1, t2

  integer :: nframes

  !----------------------------------------------------------------------------!

  call greeting('aq' // cf('stack','1'))

  if (command_argument_count() == 0) then
    call print_help(); stop
  end if

  call parse_cli()

  !----------------------------------------------------------------------------!

  nframes = merge(size(input_fn), 0, allocated(input_fn))
  if (nframes == 0) error stop "no input files!"

  cfg_resampling = cfg_resampling .and. cfg_align_frames
  if (cfg_resampling) print '(a12,": ",f6.2)', 'resampling', resample_factor

  !----------------------------------------------------------------------------!

  actual_job: block

    type(image_frame_t), dimension(:), allocatable :: frames
    type(image_frame_t) :: frame_bias, frame_dark, frame_flat
    logical, allocatable :: fix_mask(:,:)
    integer :: i, errno
    real(buf_k), allocatable :: buffers_to_stack(:,:,:)
    integer :: nx, ny


    read_calibration_frames: block
      if (bias_fn /= "") then
        if (strategy == "bias") print '(a)', cf("Achtung!", "1") // " why subtract bias from bias?"
        call frame_bias % read_fits(bias_fn)
        print '(a12,": ",a)', 'bias', trim(bias_fn)
      end if

      if (dark_fn /= "") then
        if (strategy == "dark") &
        &   print '(a)', cf("Achtung!", "1") // " why subtract dark from dark?"
        call frame_dark % read_fits(dark_fn)
        print '(a12,": ",a)', 'dark', trim(dark_fn)

        if (cfg_dark_is_dirty .and. allocated(frame_bias % data)) then
          frame_dark%data = frame_dark%data - frame_bias%data
          cfg_dark_is_dirty = .false.
          print '(a)', 'dark was dirty but now is clean'
        end if

        if (cfg_correct_hot .or. cfg_dark_optimize) then
          allocate(fix_mask(size(frame_dark%data, 1), size(frame_dark%data, 2)))
          call find_hot(frame_dark%data, hotpixel_threshold_sigma, fix_mask)
          call fix_hot(frame_dark%data, fix_mask)
  
          print '("found hotpixels:", i5)', count(fix_mask)
        end if
      end if

      if (flat_fn /= "") then
        if (strategy == "flat") print '(a)', cf("Achtung!", "1") // " why subtract flat from flat?"
        call frame_flat % read_fits(flat_fn)
        block
          integer :: n1, n2
          n1 = size(frame_flat % data, 1)
          n2 = size(frame_flat % data, 2)
          associate (calibarea => frame_flat % data(33:n1-32, 33:n2-32))
            frame_flat % data(:,:) = frame_flat % data &
              / average_safe(calibarea)
          end associate
        end block
        print '(a12,": ",a)', 'flat', trim(flat_fn)
      end if

    end block read_calibration_frames

    if (cfg_temperature_filter) then
      block
        use fitsheader_m, only: fhdict
        type(fhdict) :: hdr
        integer :: errno
        logical :: pass(nframes)

        print *
        print '(a, f5.1, a, f3.1, a)', "temperature filter: ", cfg_temperature_point, &
        &   "C +/- ", abs(cfg_temperature_tolerance), "C"
        
        pass(:) = .false.

        do i = 1, nframes
          call hdr % erase
          call hdr % load(input_fn(i), errno)
          if (errno /= 0) error stop
          if ('CCD-TEMP' .in. hdr) then
            associate (ccdtemp => hdr % get_real('CCD-TEMP'))
              if (abs(ccdtemp - cfg_temperature_point) < abs(cfg_temperature_tolerance)) then
                pass(i) = .true.
              else
                print '("rejected", 2x, a32, 2x, "T=", f6.1, "C")', trim(input_fn(i)), ccdtemp
              end if
            end associate
          end if
        end do

        input_fn = pack(input_fn, pass)
        print '(i0, " out of ", i0, " frames left")', size(input_fn), nframes
        nframes = size(input_fn)
      end block
    end if

    if (nframes == 0) error stop "no frames to stack"

    allocate(frames(nframes))
    
    print *
    print '(a32, a9, a7, a9)', 'FILENAME', 'AVG', 'STD', 'EXPOS'

    read_frames_loop: do i = 1, nframes

      errno = 0

      associate (cur_frame => frames(i))

        call cur_frame % read_fits(input_fn(i), errno)

        if (errno /= 0) then
          print '("problem opening file: ", a)', trim(input_fn(i))
          error stop
        end if

        if (i == 1) then
          ny = size(cur_frame % data, 1)
          nx = size(cur_frame % data, 2)
        end if

        if (any(shape(cur_frame % data) /= [ny, nx])) &
            error stop "images have different dimensions"

        if (allocated(frame_bias % data)) then
          if (any(shape(cur_frame % data) /= shape(frame_bias % data))) &
            error stop "image and bias frame have different dimensions"
          cur_frame % data(:,:) = cur_frame % data(:,:) - frame_bias % data(:,:)
        end if

        if (allocated(frame_dark % data) .and. .not. cfg_correct_hot_only) then

          if (any(shape(cur_frame % data) /= shape(frame_dark % data))) &
            error stop "image and dark frame have different dimensions"

          ! perform dark scaling for only clean dark (without bias)
          if (.not. cfg_dark_is_dirty) then
            dark_scaling: block
  
              real(buf_k) :: a, av, sd, amax
              logical, allocatable :: darkopt_msk(:,:)

              a = 1.0
              amax = 10.0
              
              ! if exposures are given for dark and frame, scale accordingly
              if (('EXPTIME' .in. frame_dark % hdr) .and. ('EXPTIME' .in. cur_frame % hdr)) then
                a = cur_frame % hdr % get_real('EXPTIME') / frame_dark % hdr % get_real('EXPTIME')
                amax = 2 * a
              endif

              if (cfg_dark_optimize) then
                
                ! we remove hotpixels before optimization
                if (allocated(fix_mask)) then
                  call fix_hot(cur_frame % data, fix_mask)
                else
                  error stop 'enable hotpixels please'
                end if
                
                if (darkopt_sigma > 0) then
                  allocate(darkopt_msk(ny, nx))
                  darkopt_msk(:,:) = .true.
                  call outliers(cur_frame % data, darkopt_msk, darkopt_sigma, 10, av, sd)
                  print '(f0.1, "% used for dark optimization")', real(count(darkopt_msk)) / (nx * ny) * 100
                  call optimize_dark_frame_fast(cur_frame % data, frame_dark%data, a, darkopt_msk)
                else
                  call optimize_dark_frame_fast(cur_frame % data, frame_dark%data, a)
                end if

                if (a < 0) a = 0
                if (a > amax) a = amax
              end if 
              
              write (*, '(a, f10.3)') 'dark scaling=', a
              cur_frame % data(:,:) = cur_frame % data(:,:) - a * frame_dark%data(:,:)
            end block dark_scaling
          else
            ! dark is dirty = no bias was given. subtract without scaling
            cur_frame % data(:,:) = cur_frame % data(:,:) - frame_dark%data(:,:)
          end if
        end if
          
        if (allocated(frame_flat % data)) then
          if (any(shape(cur_frame % data) /= shape(frame_flat % data))) &
            error stop "image and flat frame have different dimensions"
          cur_frame % data(:,:) = cur_frame % data(:,:) / frame_flat % data(:,:)
        end if

        where (.not. ieee_is_normal(cur_frame % data)) cur_frame % data = 0
        
        ! fix hot pixels second time (post-flatfield)
        if (allocated(fix_mask)) then
          call fix_hot(cur_frame % data, fix_mask)
        end if

        ! print some frame statistics for quick check
        frame_stats: block
          real(buf_k) :: avg, std

          call avsd(cur_frame % data, avg, std)

          print '(a32, f9.1, f7.1, f9.2)', trim(input_fn(i)), &
          &     avg, std, cur_frame % hdr % get_real('EXPTIME')
        end block frame_stats

      end associate

    end do read_frames_loop

    if (method == 'sigclip' .and. nframes < 4) then
      method = 'average'
      if ((strategy == 'bias' .or. strategy == 'dark') .and. nframes == 3) then
        method = 'median'
      end if
      print '("warning: too few frames; stacking method changed to ",a)', trim(method)
    end if

    print *

    if (cfg_process_only) then
      print '("' // cf('processing ",i0," frames','1') // '")', nframes
    else
      print '("' // cf('stacking ",i0," frames using ",a,"','1') // '")', nframes, trim(method)
    end if

    if (cfg_align_frames .and. (nframes > 1 .or. ref_fn /= "")) then
      print '(a,a,a)', 'ALIGN (', trim(align_method) ,') STARTED'
      align_frames: block
        use new_align
        use polygon_matching, only: find_transform_polygons
        type(source_t), dimension(:), allocatable :: lst0, lst
        type(align_params_t) :: align_params
        integer :: i, istart, errno
        class(transform_t), allocatable :: tx
        real(r64_k) :: r0

        ! r0 is roughly half of frame's dimension
        r0 = sqrt(real(nx, kind=r64_k)**2 + real(ny, kind=r64_k)**2) / sqrt(8.0_r64_k)
        align_params % scale = r0
        align_params % prealign_polygon = cfg_prealign_polygon

        if (cfg_resampling) then
          print '("WARNING ", a)', 'resampling may require a lot of memory'
          allocate(buffers_to_stack(nint(resample_factor * ny), nint(resample_factor * nx), nframes))
          buffers_to_stack(:,:,:) = 0
        else
          allocate(buffers_to_stack(ny, nx, nframes))
        end if

        if (ref_fn /= "") then
          read_ref_frame: block
            type(image_frame_t) :: imref

            call imref % read_fits(ref_fn)
            call register_stars(imref % data(:,:), lst0)

            istart = 1
          end block read_ref_frame
        else
          findstar_initial: block
            type(transform_xyr_t) :: ity

            call register_stars(frames(1) % data, lst0)

            if (cfg_resampling) then
              call project_bilinear(ity, frames(1) % data, buffers_to_stack(:,:,1), resample_factor)
            else
              call project_bilinear(ity, frames(1) % data, buffers_to_stack(:,:,1))
            end if

            istart = 2
          end block findstar_initial
        end if

        call cpu_time(t1)
        !$omp parallel do private(i, lst, tx) shared(buffers_to_stack)
        do i = istart, nframes
          ! register the stars
          call register_stars(frames(i) % data, lst)
          call classic_align(lst0, lst, align_method, align_params, tx, errno, cfg_verbose)
          if (errno /= 0) error stop "align error"
          
          !$omp critical
          margin = max(margin, check_corners(tx, nx, ny) + 2)
          
          print '("ALIGN ",a," frame(",i2,") found ",i4," stars")', trim(align_method), i, size(lst)
          print '(" solution(",i2,") =", *(f8.2))', i, tx % vec(:tx%npar())
          print '("margin = ", i0)', margin
          !$omp end critical
          
          if (cfg_resampling) then
            call project_bilinear(tx, frames(i) % data, buffers_to_stack(:,:,i), resample_factor)
          else
            call project_bilinear(tx, frames(i) % data, buffers_to_stack(:,:,i))
          end if

          deallocate(tx)

        end do
        !$omp end parallel do
        call cpu_time(t2)
        print perf_fmt, 'align', t2 - t1

      end block align_frames
    else
      allocate(buffers_to_stack(ny, nx, nframes))
      do i = 1, nframes
        buffers_to_stack(:,:,i) = frames(i) % data
      end do
    end if

    fix_nans: block
      integer :: i, j, k
      do k = 1, size(buffers_to_stack, 3)
        do j = 1, size(buffers_to_stack, 2)
          do i = 1, size(buffers_to_stack, 1)
            if (ieee_is_normal(buffers_to_stack(i,j,k))) cycle
            print *, "warning: nan found in buffer ", k, &
              "pixel (", j, ", ", i, ")"
            buffers_to_stack(i,j,k) = 0
          end do
        end do
      end do
    end block fix_nans

    if (cfg_normalize) then
      call cpu_time(t1)
      call normalize_offset_gain(buffers_to_stack(:, :, 1:nframes), &
        ceiling(margin * merge(resample_factor, 1.0_r64_k, cfg_resampling)))
      call cpu_time(t2)
      print perf_fmt, 'norm', t2 - t1
    end if
    
    print *

    if (cfg_process_only) then
      save_processed: block
        integer :: i
        do i = 1, nframes
          frames(i) % data = buffers_to_stack(:,:,i) 
        end do
        if (nframes == 1 .and. output_fn /= "") then
          print '(a,a)', 'writing output file: ', trim(output_fn)
          call frames(1) % write_fits(output_fn)
        else
          if (output_suff == "") output_suff = "_pp"
          print '(a,i0,a,a)', 'writing ', nframes, ' processed files with suffix: ', trim(output_suff)
          do i = 1, nframes
            call frames(i) % hdr % add('AQLVER', version)
            call frames(i) % write_fits(add_suffix(frames(i) % fn, output_suff))
          end do
        end if
      end block save_processed
    else
      block
        type(image_frame_t) :: frame_out
        character(len=:), allocatable :: output_fn_clean
        
        call stack_frames(strategy, method, frames(1:nframes), buffers_to_stack(:, :, 1:nframes), frame_out)

        output_fn_clean = select_output_filename(output_fn, strategy)
        print '(a,a)', 'writing output file: ', trim(output_fn_clean)
        call frame_out % write_fits(output_fn_clean)
        
      end block
    end if

  end block actual_job

  !----------------------------------------------------------------------------!

contains

  !----------------------------------------------------------------------------!
  ! parse the command line args
  ! note: this subroutine fills the global variables

  subroutine parse_cli
    integer :: i, errno, skip
    character(len = 256) :: arg, buf

    skip = 0

    scan_cli: do i = 1, command_argument_count()

      if (skip > 0) then
        skip = skip - 1; cycle scan_cli
      end if

      call get_command_argument(i, arg)

      ! in the first argument the strategy might be given
      if ( i == 1 ) then
        select case (arg)

        case ("bias")
          strategy = "bias"
          method = 'sigclip'
          cycle scan_cli

        case ("dark")
          strategy = "dark"
          method = 'sigclip'
          cycle scan_cli

        case ("flat")
          strategy = "flat"
          cycle scan_cli

        case ("process")
          strategy = "process"
          cfg_align_frames = .false.
          cfg_process_only = .true.
          cycle scan_cli

        case ("align")
          strategy = "process"
          cfg_align_frames = .true.
          cfg_process_only = .true.
          cycle scan_cli

        case ("final")
          strategy = "final"
          cfg_align_frames = .true.
          cfg_normalize = .true.
          method = "sigclip"
          cycle scan_cli

        end select
      end if

      ! here we deal with the rest of possible args (other than strategy)
      select case (arg)

      case ("-median")
        method = "median"

      case ("-average")
        method = "average"

      case ("-sigclip")
        method = "sigclip"
        if (strategy /= 'bias' .and. strategy /= 'dark') &
        &     cfg_normalize = .true.

      case ("-nostack", "-no-stack")
        cfg_process_only = .true.

      case ("-align")
        cfg_align_frames = .true.

        call get_command_argument(i+1, buf)
        if (buf == 'polygon' .or. buf == 'xyr' .or. buf == 'affine') then
          align_method = buf
          skip = 1
        end if

      case ("-prealign-polygon")
        cfg_prealign_polygon = .true.

      case ("-norm", "-normalize")
        cfg_normalize = .true.

      case ("-resample")
        cfg_resampling = .true.
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) resample_factor
        if (errno == 0 .and. resample_factor >= 1.0 .and. resample_factor <= 3.0) then
          skip = 1
        else
          resample_factor = 1.5
        end if

      case ("-temp", "-T")
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) cfg_temperature_point

        if (errno == 0) then
          cfg_temperature_filter = .true.
          skip = 1
        else
          error stop "command line: temperature point in celsius (float) expected"
        end if

        call get_command_argument(i + 2, buf)
        read (buf, *, iostat = errno) cfg_temperature_tolerance

        if (errno == 0) then
          skip = 2
        else
          cfg_temperature_tolerance = 0.5
        end if

      case ("-o", "-output")
        call get_command_argument(i + 1, buf)
        if (is_valid_fn_arg(buf)) then
          output_fn = buf; skip = 1
        else
          error stop "command line: output file name expected"
        end if

      case ("-suffix", "-S")
        call get_command_argument(i + 1, buf)
        if (buf /= '' .and. index(trim(buf), ' ') == 0) then
          output_suff = buf; skip = 1
        else
          error stop "command line: suffix expected"
        end if

      case ("-bias")
        call get_command_argument(i + 1, buf)
        if (is_valid_fn_arg(buf)) then
          bias_fn = buf; skip = 1
        else
          error stop "command line: bias file name expected"
        end if

      case ("-flat")
        call get_command_argument(i + 1, buf)
        if (is_valid_fn_arg(buf)) then
          flat_fn = buf; skip = 1
        else
          error stop "command line: flat file name expected"
        end if

      case ("-dark")
        call get_command_argument(i + 1, buf)
        if (is_valid_fn_arg(buf)) then
          dark_fn = buf; skip = 1
        else
          error stop "command line: dark file name expected"
        end if

      case ("-ref", "-reference")
        call get_command_argument(i + 1, buf)
        if (is_valid_fn_arg(buf)) then
          ref_fn = buf; skip = 1
        else
          error stop "command line: reference frame file name expected"
        end if

      case ("-hot")
        cfg_correct_hot = .true.

        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) hotpixel_threshold_sigma
        if (errno == 0) skip = 1

      case ("-no-hot")
        cfg_correct_hot = .false.

      case ("-hot-only")
        cfg_correct_hot_only = .true.
      case ("-no-hot-only")
        cfg_correct_hot_only = .false.

      case("-darkopt")
        cfg_dark_optimize = .true.
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) darkopt_sigma
        if (errno == 0) skip = 1
        if (errno /= 0) darkopt_sigma = 5
      case("-no-darkopt")
        cfg_dark_optimize = .false.

      case ("-dirty-dark")
        cfg_dark_is_dirty = .true.
      case ("-no-dirty-dark")
        cfg_dark_is_dirty = .false.

      case("-cfa")
        cfg_is_cfa = .true.

      case("-verbose")
        cfg_verbose = .true.

      case ("-h", "-help")
        call print_help(); stop

      case default
        if (.not. is_valid_fn_arg(arg)) error stop "command line: unknown option: " // trim(arg)
        if (allocated(input_fn)) then
          input_fn = [input_fn, arg]
        else
          input_fn = [arg]
        end if

      end select
    end do scan_cli
  end subroutine parse_cli

  ! a little helper function to check if parameter of an argument
  ! is good (for example: -o filename)

  pure logical function is_valid_fn_arg(arg) result(isok)
    character(len = *), intent(in) :: arg
    character(len = :), allocatable :: argl
    if (len_trim(arg) > 0) then
      argl = adjustl(arg)
      isok = argl == '-' .or. argl == '--' .or. argl(1:1) /= '-'
    else
      isok = .false.
    end if
  end function

  !----------------------------------------------------------------------------!
  ! print the help

  subroutine print_help
    use globals, only: fmthlp
    print '(a)', 'usage: aqstack [STRATEGY] [OPTIONS] FILE1 [FILE2 ...] -o OUTPUT'
    print '(a)', 'STRATEGY can be: bias, dark, flat, process, align, final'
    print fmthlp,  '-o/-output FILENAME', 'specifies the output filename'
    print fmthlp,  '-average', 'stack by average value'
    print fmthlp,  '-median', 'stack by median'
    print fmthlp,  '-sigclip', 'stack by 3-sigma clipped average'
    print fmthlp,  '-align [METHOD]', 'align frames. METHOD can be:', &
    &     'polygon: quadrangle matching -- only rot&transl', &
    &     'xyr: rotation and translation', &
    &     'affine {def.} use poly, then gravity to find affine (linear stretch)'
    print fmthlp,  '-prealign-polygon', 'prealignment step using polygon matching.', &
    &     'Use when frames are strongly rotated (meridian flip.)'
    print fmthlp,  '-ref FILENAME', 'align to this frame rather than first frame'
    print fmthlp,  '-resample [FACTOR=2]', 'resample before stacking (only with -align)', &
    &     'FACTOR is scale to be applied'
    print fmthlp,  '-norm[alize]', 'normalize to average before stacking'
    print fmthlp,  '-no-stack', 'process but do not stack images'
    print fmthlp,  '-suffix/-S SUFFIX', 'suffix that will be added to file names', &
    &     'when using -nostack {def.: _pp}'
    print fmthlp,  '-temp/-T TEMP [DT=0.5]', 'stack only frames with given CCD temperature', &
    &     'DT gives allowed deviation in temperature', 'in Celsius'
    print fmthlp,  '-bias FILENAME', 'subtract this master bias'
    print fmthlp,  '-flat FILENAME', 'remove this master flat'
    print fmthlp,  '-dark FILENAME', 'remove this master dark'
    print fmthlp,  '[-no]-hot [SIGMA=5.0]', 'find hot pixels on dark and correct them', &
    &     'in the image frames (if dark is given) {def.: ON}'
    print fmthlp,  '[-no]-hot-only', 'do not remove dark, just correct hot pixels', '{def.: OFF}'
    print fmthlp,  '[-no]-darkopt [SIGMA=5.0]', 'optimize dark to minimize correlation', &
    &     'if sigma is nonzero, only background will be used.', &
    &     'SIGMA=0 forces to use all pixels {def.: OFF}'
    print fmthlp,  '[-no]-dirty-dark', 'subtract bias from dark (only if not done before!)', &
    &     '{def.: OFF}'
    print fmthlp,  '-verbose', 'verbose output'
  end subroutine print_help

  !----------------------------------------------------------------------------!

end program
