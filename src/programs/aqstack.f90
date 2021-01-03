program aqstack

  !----------------------------------------------------------------------------!

  use globals
  use framehandling
  use statistics
  use stacking
  use hotpixels
  use findstar, only: extended_source
  use iso_fortran_env, only: real64, stderr => error_unit

  !----------------------------------------------------------------------------!

  implicit none

  ! allowed values: average, median, sigclip
  character(len = 32) :: method = "average", strategy = ""
  character(len = 256) :: output_fn = "", ref_fn = "", &
        bias_fn = "", dark_fn = "", flat_fn = "", fix_fn = ""
  character(len = 64) :: output_suff = ""
  character(len = 256), allocatable :: input_fn(:)
  logical :: cfg_align_frames = .false.
  logical :: cfg_process_only = .false.
  logical :: cfg_normalize = .false.
  logical :: cfg_correct_hot = .true., cfg_correct_hot_only = .true.
  logical :: cfg_resampling = .false.
  logical :: cfg_temperature_filter = .false.
  real(fp) :: resample_factor = 1.5, cfg_temperature_point = 0, cfg_temperature_tolerance = 0.5
  integer :: margin = 10, margins(2,2) = 0
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
    real(fp), allocatable, target :: buffer(:,:,:)
    type(image_frame_t) :: frame_bias, frame_dark, frame_flat
    logical, allocatable :: fix_mask(:,:)
    integer :: i, errno
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

        print *, 'warning: dark was given, but will not be corrected (unimplemented)'

        if (cfg_correct_hot) then
          allocate(fix_mask(size(frame_dark%data, 1), size(frame_dark%data, 2)))
          call find_hot(frame_dark%data, fix_mask)
  
          print '("found hotpixels:", i5)', count(fix_mask)
        end if
      end if

      if (flat_fn /= "") then
        if (strategy == "flat") print '(a)', cf("Achtung!", "1") // " why subtract flat from flat?"
        call frame_flat % read_fits(flat_fn)
        associate (n1 => size(frame_flat % data, 1), n2 => size(frame_flat % data, 2))
          associate (calibarea => frame_flat % data(33:n1-32, 33:n2-32))
            frame_flat % data(:,:) = frame_flat % data &
              / average_safe(calibarea)
          end associate
        end associate
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
            associate (ccdtemp => hdr % get_float('CCD-TEMP'))
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

      if (i == 1) then
        call read_fits_naxes(input_fn(i), nx, ny, errno)

        if (errno /= 0) then
          print '("problem opening file: ", a)', trim(input_fn(i))
          cycle read_frames_loop
        end if

        allocate(buffer(nx, ny, nframes))

      end if

      associate (cur_frame => frames(i), cur_buffer => buffer(:,:,i))

        cur_frame % data => cur_buffer
        call cur_frame % hdr % erase()
        call cur_frame % read_fits(input_fn(i), errno)

        if (errno /= 0) then
          print '("problem opening file: ", a)', trim(input_fn(i))
          error stop
        end if

        if (associated(frame_bias % data)) then
          cur_buffer(:,:) = cur_buffer(:,:) - frame_bias % data(:,:)
        end if

        if (associated(frame_flat % data)) then
          cur_buffer(:,:) = cur_buffer(:,:) / frame_flat % data(:,:)
        end if

        if (allocated(fix_mask)) then
          call fix_hot(cur_buffer, fix_mask)
        end if

        ! print some frame statistics for quick check
        frame_stats: block
          real(fp) :: avg, std

          call avsd(cur_buffer, avg, std)

          print '(a32, f9.1, f7.1, f9.2)', trim(input_fn(i)), &
          &     avg, std, cur_frame % hdr % get_float('EXPTIME')
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
      print '(a)', 'ALIGN STARTED'
      align_frames: block
        use new_align
        real(fp), allocatable :: buf_copy(:,:), buffer_resample(:,:,:)
        type(extended_source), dimension(:), allocatable :: lst0, lst
        integer :: i, istart
        class(transform_t), allocatable :: tx, tx0

        allocate(transform_xyr_t :: tx0)
        tx0 % r0 = 0.33 * sqrt(real(nx)**2 + real(ny)**2)

        if (cfg_resampling) then
          print '("WARNING ", a)', 'resampling may require a lot of memory'
          allocate(buffer_resample(nint(resample_factor * nx), nint(resample_factor * ny), nframes))
          buffer_resample(:,:,:) = 0
        else
          allocate(buf_copy(nx, ny))
        end if

        if (ref_fn /= "") then
          read_ref_frame: block
            type(image_frame_t) :: imref
            call imref % read_fits(ref_fn)
            call register_stars(imref % data(:,:), lst0)
            deallocate(imref % data)
          end block read_ref_frame
          istart = 1
        else
          findstar_initial: block
            type(transform_xyr_t) :: ity
            call register_stars(buffer(:,:,1), lst0)
            istart = 2
            if (cfg_resampling) then
              ! scale the first frame
              call ity % project(buffer(:,:,1), buffer_resample(:,:,1), resample_factor)
            end if
          end block findstar_initial
        end if

        call cpu_time(t1)
        !$omp parallel do private(i, lst, buf_copy, tx)
        do i = istart, nframes
          allocate(tx, source = tx0)

          ! find the transform between frames
          call register_stars(buffer(:,:,i), lst)
          call tx % align(lst0, lst)

          print '("ALIGN frame(",i2,") found ",i4," stars")', i, size(lst)
          print '(" solution(",i2,") =", *(f8.2))', i, tx % vec(1 : tx % npar())

          !$omp critical
          margin = max(margin, check_corners(tx, nx, ny))
          !$omp end critical

          ! when not resampling, we have only one copy of data, so we copy
          ! each frame to a temporary buffer before projection
          if (.not. cfg_resampling) then
            buf_copy(:,:) = buffer(:,:,i)
            buffer(:,:,i) = 0
            call tx % project(buf_copy, buffer(:,:,i))
          else
            call tx % project(buffer(:,:,i), buffer_resample(:,:,i), resample_factor)
          end if

          deallocate(tx)
        end do
        !$omp end parallel do
        call cpu_time(t2)
        print perf_fmt, 'align', t2 - t1

        ! if resampling was performed, replace the normal buffer with resampled
        if (cfg_resampling) then
          deallocate(buffer)
          call move_alloc(buffer_resample, buffer)
          do i = 1, nframes
            frames(i) % data => buffer(:,:,i)
          end do
        end if
      end block align_frames
    end if

    if (cfg_normalize) then
      call cpu_time(t1)
      call normalize_offset_gain(buffer(:, :, 1:nframes), margin)
      call cpu_time(t2)
      print perf_fmt, 'norm', t2 - t1
    end if
    
    print *

    if (cfg_process_only) then
      save_processed: block
      integer :: i
      character(len = 256) :: newfn

        if (nframes == 1 .and. output_fn /= "") then
          print '(a,a)', 'writing output file: ', trim(output_fn)
          call frames(1) % write_fits(output_fn)
        else
          if (output_suff == "") output_suff = "_r"
          print '(a,i0,a,a)', 'writing ', nframes, ' processed files with suffix: ', trim(output_suff)
          do i = 1, nframes
            call frames(i) % write_fits(add_suffix(frames(i) % fn, output_suff))
          end do
        end if
      end block save_processed
    else
      call stack_and_write(strategy, method, frames(1:nframes), buffer(:, :, 1:nframes), output_fn)
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

      case ("-temperature", "-T")
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

        if (errno == 0) then
          skip = 1
        else
          hotpixel_threshold_sigma = 5.0
        end if
      case ("-no-hot")
        cfg_correct_hot = .false.

      case ("-only-hot")
        cfg_correct_hot_only = .true.
      case ("-no-only-hot")
        cfg_correct_hot_only = .false.

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
    use globals, only: hlp_fmt, hlp_fmtc
    print '(a)', 'usage: aqstack [STRATEGY] [OPTIONS] FILE1 [FILE2 ...] -o OUTPUT'
    print '(a)', 'STRATEGY can be: bias, dark, flat, process, align, final'
    print hlp_fmt,  '-o/-output FILENAME', 'specifies the output filename'
    print hlp_fmt,  '-average', 'stack by average value'
    print hlp_fmt,  '-median', 'stack by median'
    print hlp_fmt,  '-sigclip', 'stack by 3-sigma clipped average'
    print hlp_fmt,  '-align', 'align frames'
    print hlp_fmt,  '-ref FILENAME', 'align to this frame rather than first frame'
    print hlp_fmt,  '-resample [FACTOR=1.5]', 'resample before stacking (only with -align)'
    print hlp_fmtc, 'FACTOR is scale to be applied (default: 1.5)'
    print hlp_fmt,  '-norm[alize]', 'normalize to average before stacking'
    print hlp_fmt,  '-no-stack', 'process but do not stack images'
    print hlp_fmt,  '-suffix/-S FILENAME', 'suffix that will be added to file names'
    print hlp_fmtc, 'when using -nostack (default: _r)'
    print hlp_fmt,  '-T TEMP [TOLER=0.5]', 'stack only frames with given CCD temperature'
    print hlp_fmtc, 'TOLER gives allowed deviation in temperature (default: 0.5)'
    print hlp_fmt,  '-bias FILENAME', 'subtract this master bias'
    print hlp_fmt,  '-flat FILENAME', 'remove this master flat'
    print hlp_fmt,  '-dark FILENAME', 'remove this master dark'
    print hlp_fmt,  '[-no]-hot [SIGMA=5.0]', 'find hot pixels on dark and correct them'
    print hlp_fmtc, 'in the image frames (enabled by default if dark is given)'
    print hlp_fmt,  '[-no]-only-hot', 'do not remove dark, just correct hot pixels'
  end subroutine print_help

  !----------------------------------------------------------------------------!

end program
