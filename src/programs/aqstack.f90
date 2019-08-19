program aqstack

  !----------------------------------------------------------------------------!

  use globals
  use framehandling
  use statistics
  use findstar, only: extended_source

  !----------------------------------------------------------------------------!

  implicit none

  ! allowed values: average, median, sigclip
  character(len = 32) :: method = "average", strategy = ""
  character(len = 256) :: output_fn = "", ref_fn = "", &
        bias_fn = "", dark_fn = "", flat_fn = ""
  character(len = 64) :: output_suff = ""
  character(len = 256), allocatable :: input_fn(:)
  logical :: cfg_align_frames = .false.
  logical :: cfg_clean_cosmics = .false.
  logical :: cfg_process_only = .false.
  logical :: cfg_auto_invert = .false.
  logical :: cfg_normalize = .false.
  logical :: cfg_estimate_noise = .false.
  logical :: cfg_resampling = .false.
  real(fp) :: resample_factor = 1.5
  integer :: margin = 64

  integer :: nframes

  !----------------------------------------------------------------------------!

  allocate(input_fn(0))

  !----------------------------------------------------------------------------!

  call greeting('aq' // cf('stack','1'))

  if (command_argument_count() == 0) then
    call print_help(); stop
  end if

  !----------------------------------------------------------------------------!

  parse_cli: block
    integer :: i, nargs
    character(len = 256) :: arg, buf
    logical, allocatable :: command_argument_mask(:)

    nargs = command_argument_count()
    allocate(command_argument_mask(nargs + 1))
    command_argument_mask(:nargs) = .true.
    command_argument_mask(nargs+1:) = .false.

    scan_cli: do i = 1, command_argument_count()

      if (.not. command_argument_mask(i)) cycle scan_cli
      call get_command_argument(i, arg)

      select case (arg)

      case ("bias")
        if (i == 1) then
          strategy = "bias"
          method = 'median'
          cfg_estimate_noise = .true.
          command_argument_mask(i) = .false.
        end if

      case ("dark")
        if (i == 1) then
          strategy = "dark"
          cfg_clean_cosmics = .true.
          command_argument_mask(i) = .false.
        end if

      case ("flat")
        if (i == 1) then
          strategy = "flat"
          command_argument_mask(i) = .false.
        end if

      case ("process")
        if (i == 1) then
          strategy = "process"
          cfg_align_frames = .false.
          cfg_clean_cosmics = .true.
          cfg_process_only = .true.
          command_argument_mask(i) = .false.
        end if

      case ("final")
        if (i == 1) then
          strategy = "final"
          cfg_align_frames = .true.
          cfg_clean_cosmics = .true.
          cfg_auto_invert = .true.
          command_argument_mask(i) = .false.
        end if

      case ("-median")
        method = "median"
        command_argument_mask(i) = .false.

      case ("-average")
        method = "average"
        command_argument_mask(i) = .false.

      case ("-estimate-noise")
        cfg_estimate_noise = .true.
        command_argument_mask(i) = .false.

      case ("-nostack")
        cfg_process_only = .true.
        command_argument_mask(i) = .false.

      case ("-align")
        cfg_align_frames = .true.
        command_argument_mask(i) = .false.

      case ("-norm", "-normalize")
        cfg_normalize = .true.
        command_argument_mask(i) = .false.

      case ("-resample")
        cfg_resampling = .true.
        command_argument_mask(i) = .false.

      case ("-factor")
        if (.not. command_argument_mask(i + 1)) then
          error stop "resampling factor (float) expected"
        else
          call get_command_argument(i + 1, buf)
          read (buf, *) resample_factor
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-o","-output")
        if (.not. command_argument_mask(i + 1)) then
          error stop "output file name expected"
        else
          call get_command_argument(i + 1, output_fn)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-suffix", "-S")
        if (.not. command_argument_mask(i + 1)) then
          error stop "suffix expected"
        else
          call get_command_argument(i + 1, output_suff)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-bias")
        if (.not. command_argument_mask(i + 1)) then
          error stop "bias file name expected"
        else
          call get_command_argument(i + 1, bias_fn)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-flat")
        if (.not. command_argument_mask(i + 1)) then
          error stop "flat file name expected"
        else
          call get_command_argument(i + 1, flat_fn)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-ref", "-reference")
        if (.not. command_argument_mask(i + 1)) then
          error stop "reference frame file name expected"
        else
          call get_command_argument(i + 1, ref_fn)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-h", "-help")
        call print_help(); stop

      case default
        if (arg(1:1) == '-') error stop "unknown option: " // trim(arg)

      end select
    end do scan_cli

    scan_inpfiles: do i = 1, command_argument_count()
      if (command_argument_mask(i)) then
        call get_command_argument(i, arg)
        input_fn = [input_fn, arg]
      end if
    end do scan_inpfiles

  end block parse_cli

  !----------------------------------------------------------------------------!

  nframes = size(input_fn)
  if (nframes == 0) error stop "no input files!"

  if (output_fn == "") then
    if (strategy /= "") then
      output_fn = trim(strategy) // ".fits"
    else
      output_fn = "out.fits"
    end if
  end if
  if (output_suff == "") output_suff = "R"

  verify_cli_arguments: block
    character(len = *), parameter :: fmt = '(a12, ": ", g0)'
    integer :: i

    write (stderr, fmt) 'frames', nframes
    write (stderr, fmt) 'method', trim(method)
    write (stderr, fmt) 'output', trim(output_fn)
  end block verify_cli_arguments

  !----------------------------------------------------------------------------!

  actual_job: block

    type(image_frame_t), dimension(:), allocatable :: frames
    real(fp), allocatable, target :: buffer(:,:,:)
    type(image_frame_t) :: frame_bias, frame_dark, frame_flat
    integer :: i, n, errno
    integer :: nx, ny


    read_calibration_frames: block
      if (bias_fn /= "") then
        if (strategy == "bias") error stop "why subtract bias from bias?"
        call frame_bias % read_fits(bias_fn)
        write (0, '(a12,": ",a)') 'bias', trim(bias_fn)
      end if

      if (dark_fn /= "") then
        if (strategy == "dark") error stop "why subtract dark from dark?"
        call frame_dark % read_fits(dark_fn)
        write (0, '(a12,": ",a)') 'dark', trim(dark_fn)
      end if

      if (flat_fn /= "") then
        if (strategy == "flat") error stop "why subtract flat from flat?"
        call frame_flat % read_fits(flat_fn)
        associate (n1 => size(frame_flat % data, 1), n2 => size(frame_flat % data, 2))
          associate (calibarea => frame_flat % data(33:n1-32, 33:n2-32))
            frame_flat % data(:,:) = frame_flat % data &
              / average_safe(calibarea)
          end associate
        end associate
        write (0, '(a12,": ",a)') 'flat', trim(flat_fn)
      end if
    end block read_calibration_frames

    n = 0
    allocate(frames(nframes))

    read_frames_loop: do i = 1, nframes

      errno = 0

      if (n == 0) then
        call read_fits_naxes(input_fn(i), nx, ny, errno)

        if (errno /= 0) then
          write (0, '("problem opening file: ", a)') trim(input_fn(i))
          cycle read_frames_loop
        end if

        allocate(buffer(nx, ny, nframes))
      end if

      associate (cur_frame => frames(n + 1), cur_buffer => buffer(:, :, n + 1))

        cur_frame % data => cur_buffer
        call cur_frame % read_fits(input_fn(i), errno)

        if (errno /= 0) then
          write (0, '("problem opening file: ", a)') trim(input_fn(i))
          cycle read_frames_loop
        end if

        if (associated(frame_bias % data)) then
          cur_buffer(:,:) = cur_buffer(:,:) - frame_bias % data(:,:)
        end if

        if (associated(frame_flat % data)) then
          cur_buffer(:,:) = cur_buffer(:,:) / frame_flat % data(:,:)
        end if

        ! print some frame statistics for quick check
        frame_stats: block
          real(fp) :: avg, std
          integer :: nn

          nn = size(cur_buffer)
          avg = sum(cur_buffer) / nn
          std = sqrt(sum((cur_buffer - avg)**2) / (nn - 1))

          if (n == 0) print '(a24, a10, a9, a10, a9)', 'FILENAME', 'AVG', 'STD', 'EXPOS', 'TEMP'

          if (ieee_is_normal(cur_frame % ccdtemp)) then
            print '(a24, f10.1, f9.1, f10.2, f9.2)', trim(input_fn(i)), avg, std, cur_frame % exptime, cur_frame % ccdtemp
          else
            print '(a24, f10.1, f9.1, f10.2, a9)', trim(input_fn(i)), avg, std, cur_frame % exptime, '--'
          end if
        end block frame_stats

      end associate

      n = n + 1
    end do read_frames_loop

    if (n == 0) then
      error stop "no frames to stack"
    end if

    if (cfg_clean_cosmics) then
      write (0, '(a)') 'warning: cosmic rays clean not implemented yet'
    end if

    if (cfg_auto_invert) then
      write (0, '(a)') 'warning: auto rotation not implemented yet'
    end if

    if (cfg_align_frames .and. (n > 1 .or. ref_fn /= "")) then
      align_frames: block
        use legacy_align, only: align_xyr, improject
        real(fp), allocatable :: buf_copy(:,:), buffer_resample(:,:,:)
        type(extended_source), dimension(:), allocatable :: lst0, lst
        real(fp) :: mx(2,3)
        integer :: i, istart

        if (cfg_resampling) then
          write (0, '("WARNING ", a)') 'resampling may require a lot of memory'
          allocate(buffer_resample(nint(resample_factor * nx), nint(resample_factor * ny), n))
          buffer_resample(:,:,:) = 0
        else
          allocate(buf_copy(nx, ny))
        end if

        if (ref_fn /= "") then
          read_ref_frame: block
            type(image_frame_t) :: imref
            call imref % read_fits(ref_fn)
            call findstar_local(imref % data(:,:), lst0)
            deallocate(imref % data)
          end block read_ref_frame
          istart = 1
        else
          call findstar_local(buffer(:,:,1), lst0)
          istart = 2
          if (cfg_resampling) then
            ! neutral transform (identity)
            mx(:,1) = [0, 0]
            mx(:,2) = [1, 0]
            mx(:,3) = [0, 1]
            ! scale the first frame
            call improject(buffer(:,:,1), mx, buffer_resample(:,:,1), resample_factor)
          end if
        end if

        !$omp parallel do private(i, lst, buf_copy, mx)
        do i = istart, n
          ! find the transform between frames
          call findstar_local(buffer(:,:,i), lst)
          call align_xyr(lst0, lst, mx)

          write (0, '("ALIGN frame(",i2,") found ",i4," stars")') i, size(lst)
          write (0, '("newX = ",f6.1," + ",f6.3,"*X + ",f6.3,"*Y")') mx(1,:)
          write (0, '("newY = ",f6.1," + ",f6.3,"*X + ",f6.3,"*Y")') mx(2,:)

          margin = max(margin, ceiling(abs(mx(1,1) * 1.25)), ceiling(abs(mx(2,1) * 1.25)))

          ! when not resampling, we have only one copy of data, so we copy
          ! each frame to a temporary buffer before projection
          if (.not. cfg_resampling) then
            buf_copy(:,:) = buffer(:,:,i)
            buffer(:,:,i) = 0
            call improject(buf_copy, mx, buffer(:,:,i))
          else
            call improject(buffer(:,:,i), mx, buffer_resample(:,:,i), resample_factor)
          end if
        end do
        !$omp end parallel do

        ! if resampling was performed, replace the normal buffer with resampled
        if (cfg_resampling) then
          deallocate(buffer)
          call move_alloc(buffer_resample, buffer)
          do i = 1, n
            frames(i) % data => buffer(:,:,i)
          end do
        end if
      end block align_frames
    end if

    if (cfg_normalize) then
      block_normalize: block
        use statistics, only: linfit
        real(fp) :: a, b
        real(fp), allocatable :: imref(:,:), xx(:), yy(:)
        logical, allocatable :: mask(:,:)
        integer :: i, sz(3)

        sz = shape(buffer)

        ! create mean frame to normalize to
        imref = sum(buffer(:,:,1:n), 3) / n

        ! create mask which excludes edges and the brigtenst pixels
        allocate(mask(sz(1), sz(2)))
        associate (m => margin)
          associate (imc => imref(1+m : sz(1)-m, 1+m : sz(2)-m))
            mask = imref < (minval(imc) + maxval(imc)) / 2
          end associate
          mask(:m, :) = .false.; mask(sz(1)-m+1:, :) = .false.
          mask(:, :m) = .false.; mask(:, sz(2)-m+1:) = .false.
        end associate

        ! pack it into 1-d array
        xx = pack(imref, mask)
        deallocate(imref)
        allocate(yy, mold = xx)

        do i = 1, n
          yy(:) = pack(buffer(:,:,i), mask)
          call linfit(xx, yy, a, b)
          write (0, '("NORM frame(",i2,") y = ",f5.3,"x + ",f7.1)') i, a, b
          buffer(:,:,i) = (buffer(:,:,i) - b) / a
        end do
      end block block_normalize
    end if

    if (cfg_process_only) then
      save_processed: block
        integer :: i
        character(len = 256) :: newfn
        if ( n == 1 ) then
          call frames(1) % write_fits(output_fn)
        else
          do i = 1, n
            call frames(i) % write_fits(add_suffix(frames(i) % fn, output_suff))
          end do
        end if
      end block save_processed
    else
      actual_stack: block
        use statistics
        type(image_frame_t) :: frame_out
        real(fp) :: a(n)
        integer :: i,j
        allocate(frame_out % data(size(buffer, 1), size(buffer, 2)))
        select case (method)
        case ('average')
          !$omp parallel do private(i, j)
          do j = 1, size(buffer,2)
            do i = 1, size(buffer,1)
              frame_out % data(i,j) = average(buffer(i,j,1:n))
            end do
          end do
          !$omp end parallel do
        case ('median')
          !$omp parallel do private(i, j, a)
          do j = 1, size(buffer,2)
            do i = 1, size(buffer,1)
              a(:) = buffer(i,j,1:n)
              frame_out % data(i,j) = quickselect(a, (n + 1) / 2)
            end do
          end do
          !$omp end parallel do
        case default
          error stop "this averaging method is not supported"
        end select
        frame_out % exptime = average_safe(frames(1:n) % exptime)
        call frame_out % write_fits(output_fn)

        if ( cfg_estimate_noise ) then
          estimate_noise: block
            real(fp) :: noise
            integer :: i

            noise = 0
            do i = 1, n
              noise = noise + sum((buffer(:,:,i) - frame_out % data)**2) / (nx * ny)
            end do

            write (*, '("RMS = ", f10.2)') sqrt(noise / n)
          end block estimate_noise
        end if

        deallocate(frame_out % data)
      end block actual_stack
    end if


  end block actual_job

  !----------------------------------------------------------------------------!

contains

  subroutine print_help
    use globals, only: hlp_fmt, hlp_fmtc
    write (*, '(a)') 'usage: aqstack [STRATEGY] [OPTIONS] FILE1 [FILE2 ...] -o OUTPUT'
    write (*, '(a)') 'STRATEGY can be: bias, dark, flat, process, final'
    write (*, hlp_fmt) '-o/-output FILENAME', 'specifies the output filename'
    write (*, hlp_fmt) '-average', 'stack by average value'
    write (*, hlp_fmt) '-median', 'stack by median'
    write (*, hlp_fmt) '-estimate-noise', 'estimate noise while computing bias'
    write (*, hlp_fmt) '-align', 'align frames'
    write (*, hlp_fmt) '-ref FILENAME', 'align to this frame rather than first frame'
    write (*, hlp_fmt) '-resample', 'resample before stacking (only with -align)'
    write (*, hlp_fmt) '-factor FACTOR', 'resampling factor (default: 1.5)'
    write (*, hlp_fmt) '-norm[alize]', 'normalize to average before stacking'
    write (*, hlp_fmt) '-nostack', 'process but do not stack images'
    write (*, hlp_fmt) '-suffix/-S FILENAME', 'suffix that will be added to file names'
    write (*, hlp_fmtc) 'when using -nostack (default: R)'
    write (*, hlp_fmt) '-bias FILENAME', 'subtract this master bias'
    write (*, hlp_fmt) '-flat FILENAME', 'remove this master flat'
  end subroutine print_help

  subroutine findstar_local(im, lst)
    use convolutions, only: convol_fix
    use kernels, only: mexhakrn_alloc
    use findstar, only: aqfindstar

    real(fp), intent(in), contiguous :: im(:,:)
    type(extended_source), intent(out), allocatable :: lst(:)
    real(fp), allocatable :: im2(:,:), krn(:,:)
    integer :: nstars

    krn = mexhakrn_alloc(2.3_fp)

    allocate(im2(size(im,1), size(im,2)))
    call convol_fix(im, krn, im2, 'r')
    call aqfindstar(im2, lst, limit = 256)
  end subroutine

end program
