program aqstack

  !----------------------------------------------------------------------------!

  use globals
  use framehandling
  use statistics
  use findstar, only: extended_source
  use iso_fortran_env, only: real64, stderr => error_unit

  !----------------------------------------------------------------------------!

  implicit none

  ! allowed values: average, median, sigclip
  character(len = 32) :: method = "average", strategy = ""
  character(len = 256) :: output_fn = "", ref_fn = "", &
        bias_fn = "", dark_fn = "", flat_fn = ""
  character(len = 64) :: output_suff = ""
  character(len = 256), allocatable :: input_fn(:)
  logical :: cfg_align_frames = .false.
  logical :: cfg_process_only = .false.
  logical :: cfg_normalize = .false.
  logical :: cfg_find_hot = .false., cfg_correct_hot = .false.
  logical :: cfg_resampling = .false.
  logical :: cfg_temperature_filter = .false.
  real(fp) :: resample_factor = 1.5, cfg_temperature_point = 0
  integer :: margin = 32, margins(2,2) = 0
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

  if (output_fn == "") then
    if (strategy /= "") then
      output_fn = trim(strategy) // ".fits"
    else
      output_fn = "out.fits"
    end if
  end if
  if (output_suff == "") output_suff = "_R"

  if (cfg_temperature_filter) &
  &     write (stderr, '("temperature filter: ",f5.1,"C")') cfg_temperature_point

  cfg_resampling = cfg_resampling .and. cfg_align_frames
  if (cfg_resampling) write (stderr, '(a12,": ",f6.2)') 'resampling', resample_factor

  !----------------------------------------------------------------------------!

  actual_job: block

    type(image_frame_t), dimension(:), allocatable :: frames
    real(fp), allocatable, target :: buffer(:,:,:)
    type(image_frame_t) :: frame_bias, frame_dark, frame_flat
    integer :: i, nstack, errno
    integer :: nx, ny


    read_calibration_frames: block
      if (bias_fn /= "") then
        if (strategy == "bias") error stop "why subtract bias from bias?"
        call frame_bias % read_fits(bias_fn)
        write (stderr, '(a12,": ",a)') 'bias', trim(bias_fn)
      end if

      if (dark_fn /= "") then
        if (strategy == "dark") error stop "why subtract dark from dark?"
        call frame_dark % read_fits(dark_fn)
        write (stderr, '(a12,": ",a)') 'dark', trim(dark_fn)
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
        write (stderr, '(a12,": ",a)') 'flat', trim(flat_fn)
      end if
    end block read_calibration_frames

    nstack = 0
    allocate(frames(nframes))
    print '(a27, a9, a7, a9, a8)', 'FILENAME', 'AVG', 'STD', 'EXPOS', 'TEMP'

    read_frames_loop: do i = 1, nframes

      errno = 0

      if (nstack == 0 .and. .not. allocated(buffer)) then
        call read_fits_naxes(input_fn(i), nx, ny, errno)

        if (errno /= 0) then
          write (stderr, '("problem opening file: ", a)') trim(input_fn(i))
          cycle read_frames_loop
        end if

        allocate(buffer(nx, ny, nframes))
      end if

      associate (cur_frame => frames(nstack + 1), cur_buffer => buffer(:, :, nstack + 1))

        ! TODO: filter by temperature

        cur_frame % data => cur_buffer
        call cur_frame % hdr % erase()
        call cur_frame % read_fits(input_fn(i), errno)

        if (errno /= 0) then
          write (stderr, '("problem opening file: ", a)') trim(input_fn(i))
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
          character(len = 192) :: buf

          call avsd(cur_buffer, avg, std)

          if ('CCD-TEMP' .in. cur_frame % hdr) then
            associate (ccdtemp => cur_frame % hdr % get_float('CCD-TEMP'))
              write (buf, '(a27, f9.1, f7.1, f9.2, f8.2)') trim(input_fn(i)), avg, std, &
              &     cur_frame % exptime, ccdtemp
              if (cfg_temperature_filter) then
                if (abs(cfg_temperature_point - ccdtemp) > 0.5) then
                  write (*, '("' // cf('",a,"','31') // '")') trim(buf)
                  cycle read_frames_loop
                end if
              end if
              write (*, '(a)') trim(buf)
            end associate
          else
            write (buf, '(a27, f9.1, f7.1, f9.2, a8)') trim(input_fn(i)), &
            &     avg, std, cur_frame % exptime, '--'
            if (cfg_temperature_filter) then
              write (*, '("' // cf('",a,"','31') // '")') trim(buf)
              cycle read_frames_loop
            end if
            write (*, '(a)') trim(buf)
          end if
        end block frame_stats

      end associate

      nstack = nstack + 1
    end do read_frames_loop

    if (nstack == 0) then
      error stop "no frames to stack"
    end if

    if (method == 'sigclip' .and. nstack < 4) then
      method = 'average'
      if ((strategy == 'bias' .or. strategy == 'dark') .and. nstack == 3) then
        method = 'median'
      end if
      write (stderr, '("warning: too few frames; stacking method changed to ",a)') trim(method)
    end if

    if (cfg_process_only) then
      write (stderr, '("' // cf('processing ",i0," frames, filename suffix: ",a,"','1') // '")') nstack, trim(output_suff)
    else
      write (stderr, '("' // cf('stacking ",i0," frames using ",a,"','1') // '")') nstack, trim(method)
    end if

    if (cfg_align_frames .and. (nstack > 1 .or. ref_fn /= "")) then
      write (stderr, '(a)') 'ALIGN STARTED'
      align_frames: block
        use new_align
        real(fp), allocatable :: buf_copy(:,:), buffer_resample(:,:,:)
        type(extended_source), dimension(:), allocatable :: lst0, lst
        integer :: i, istart
        class(transform_t), allocatable :: tx, tx0

        allocate(transform_xyr_t :: tx0)
        tx0 % r0 = 0.33 * sqrt(real(nx)**2 + real(ny)**2)

        if (cfg_resampling) then
          write (stderr, '("WARNING ", a)') 'resampling may require a lot of memory'
          allocate(buffer_resample(nint(resample_factor * nx), nint(resample_factor * ny), nstack))
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
        do i = istart, nstack
          allocate(tx, source = tx0)

          ! find the transform between frames
          call register_stars(buffer(:,:,i), lst)
          call tx % align(lst0, lst)

#         ifdef _DEBUG
          !$omp critical
          block
            character(len = 256) :: fn1
            integer :: jj
            write (fn1, '("stars.",i0.3,".txt")') i
            open (11, file = fn1, action = 'write')
            write (11, '(2f15.4)') (lst(jj) % x, lst(jj) % y, jj = 1, size(lst))
            close (11)
          end block
          !$omp end critical
#         endif

          write (stderr, '("ALIGN frame(",i2,") found ",i4," stars")') i, size(lst)
          write (stderr, '(" solution(",i2,") =", *(f8.2))') i, tx % vec(1 : tx % npar())

          !$omp critical
          margin = max(margin, ceiling(abs(tx % vec(1) * 1.1)) + 1, &
            ceiling(abs(tx % vec(2) * 1.1)) + 1)
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
          do i = 1, nstack
            frames(i) % data => buffer(:,:,i)
          end do
        end if
      end block align_frames
    end if

    if (cfg_normalize) then
      call cpu_time(t1)
      call normalize_offset_gain(buffer(:, :, 1:nstack))
      call cpu_time(t2)
      print perf_fmt, 'norm', t2 - t1
    end if

    if (cfg_process_only) then
      save_processed: block
        integer :: i
        character(len = 256) :: newfn
        if ( nstack == 1 ) then
          call frames(1) % write_fits(output_fn)
        else
          do i = 1, nstack
            call frames(i) % write_fits(add_suffix(frames(i) % fn, output_suff))
          end do
        end if
      end block save_processed
    else
      actual_stack: block
        type(image_frame_t) :: frame_out

        allocate(frame_out % data(size(buffer, 1), size(buffer, 2)))

        call cpu_time(t1)
        call stack_buffer(method, buffer(:, :, 1:nstack), frame_out % data)
        call cpu_time(t2)
        print perf_fmt, 'stack', t2 - t1

        write_extra_info_hdr: block

          call frame_out % hdr % add_int('NSTACK', nstack)
          call frame_out % hdr % add_str('STCKMTD', method)
          if (strategy /= '') call frame_out % hdr % add_str('FRAMETYP', strategy)

          call propagate_average_value_real(frames(1:nstack), 'EXPTIME', frame_out)
          call propagate_average_value_real(frames(1:nstack), 'CCD-TEMP', frame_out)

        end block write_extra_info_hdr

        if ((strategy == 'bias' .or. strategy == 'dark') .and. nstack > 1) then
          estimate_noise: block
            real(fp) :: rms

            rms = estimate_differential_noise(buffer)

            write (*, '("RMS = ", f10.2)') rms
            call frame_out % hdr % add_float('RMS', real(rms))
            call frame_out % hdr % add_float('STACKRMS', real(rms / sqrt(1.0_fp * nstack)))
          end block estimate_noise
        end if

        call frame_out % write_fits(output_fn)

        deallocate(frame_out % data)
      end block actual_stack
    end if

  end block actual_job

  !----------------------------------------------------------------------------!

contains

  !----------------------------------------------------------------------------!

  subroutine stack_buffer(method, buffer, buffer_out)
    use statistics, only: quickselect, sigclip2

    real(fp), intent(in) :: buffer(:,:,:)
    real(fp), intent(out) :: buffer_out(:,:)
    character(len = *), intent(in) :: method
    integer :: i, j, nstack
    real(fp) :: a(size(buffer, 3))

    nstack = size(buffer, 3)

    select case (method)
    case ('m', 'median')
      !$omp parallel do private(i, j, a)
      do j = 1, size(buffer, 2)
        do i = 1, size(buffer, 1)
          a(:) = buffer(i, j, 1:nstack)
          ! forall (k = 1:nstack) a(k) = frames(k) % data(i, j)
          buffer_out(i, j) = quickselect(a(:), (nstack + 1) / 2)
        end do
      end do
      !$omp end parallel do
    case ('s', 'sigclip')
      !$omp parallel do private(i, j, a)
      do j = 1, size(buffer, 2)
        do i = 1, size(buffer, 1)
          ! forall (k = 1:nstack) a(k) = frames(k) % data(i, j)
          ! call sigclip2(a(:), frame_out % data(i, j))
          call sigclip2(buffer(i, j, 1:nstack), buffer_out(i, j))
        end do
      end do
      !$omp end parallel do
    case default
      buffer_out(:, :) = sum(buffer(:, :, 1:nstack), 3) / nstack
    end select
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine propagate_average_value_real(frames, kw, frame_out)
    class(image_frame_t), intent(in) :: frames(:)
    class(image_frame_t), intent(inout) :: frame_out
    character(len = *) :: kw
    logical :: m(size(frames))
    real :: av
    integer :: i, errno

    m(:) = [ (frames(i) % hdr % has_key(kw), i = 1, size(frames)) ]
    if (count(m) > 0) then
      av = sum([ (merge(frames(i) % hdr % get_float(kw, errno), 0.0, m(i)), &
      &     i = 1, size(frames)) ]) / count(m)
      call frame_out % hdr % add_float(kw, av)
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  function estimate_differential_noise(buffer) result(rms)
    use iso_fortran_env, only: int64
    real(fp), intent(in) :: buffer(:,:,:)
    real(fp) :: rms
    integer :: i, n
    integer(int64) :: nxny

    nxny = size(buffer, 1) * size(buffer, 2)
    n = size(buffer, 3)

    rms = 0
    do i = 1, n - 1
      rms = rms + sum((buffer(:,:,i) - buffer(:,:,i+1))**2) / (2 * nxny)
    end do

    rms = sqrt(rms / (n - 1))
  end function

  !----------------------------------------------------------------------------!

  subroutine normalize_offset_gain(buffer)
    use statistics, only: linfit
    real(fp) :: a, b
    real(fp), allocatable :: imref(:,:), xx(:), yy(:)
    real(fp), intent(inout) :: buffer(:,:,:)
    logical, allocatable :: mask(:,:)
    integer :: i, sz(3), nstack

    sz = shape(buffer)
    nstack = sz(3)

    ! create mean frame to normalize to
    imref = sum(buffer, 3) / nstack

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

    do i = 1, nstack
      yy(:) = pack(buffer(:,:,i), mask)
      call linfit(xx, yy, a, b)
      write (stderr, '("NORM frame(",i2,") y = ",f5.3,"x + ",f7.1)') i, a, b
      buffer(:,:,i) = (buffer(:,:,i) - b) / a
    end do
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine register_stars(im, lst)
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

  !----------------------------------------------------------------------------!

  function check_corners(tx, nx, ny) result(margin)
    use new_align, only: transform_t

    class(transform_t), intent(in) :: tx
    integer, intent(in) :: nx, ny
    real(fp) :: rx, ry, sx, sy
    integer :: margin

    rx = 0.5_fp * (nx - 1)
    ry = 0.5_fp * (ny - 1)

    margin = 0
    call tx % apply(-rx, -ry, sx, sy)
    margin = max(margin, ceiling(abs(-rx - sx)), ceiling(abs(-ry - sy)))
    call tx % apply( rx, -ry, sx, sy)
    margin = max(margin, ceiling(abs( rx - sx)), ceiling(abs(-ry - sy)))
    call tx % apply( rx,  ry, sx, sy)
    margin = max(margin, ceiling(abs( rx - sx)), ceiling(abs( ry - sy)))
    call tx % apply(-rx,  ry, sx, sy)
    margin = max(margin, ceiling(abs(-rx - sx)), ceiling(abs( ry - sy)))
  end function

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
          cfg_find_hot = .true.
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

      case ("-nostack")
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

      case ("-ref", "-reference")
        call get_command_argument(i + 1, buf)
        if (is_valid_fn_arg(buf)) then
          ref_fn = buf; skip = 1
        else
          error stop "command line: reference frame file name expected"
        end if

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
    write (*, '(a)') 'usage: aqstack [STRATEGY] [OPTIONS] FILE1 [FILE2 ...] -o OUTPUT'
    write (*, '(a)') 'STRATEGY can be: bias, dark, flat, process, align, final'
    write (*, hlp_fmt) '-o/-output FILENAME', 'specifies the output filename'
    write (*, hlp_fmt) '-average', 'stack by average value'
    write (*, hlp_fmt) '-median', 'stack by median'
    write (*, hlp_fmt) '-sigclip', 'stack by 3-sigma clipped average'
    write (*, hlp_fmt) '-align', 'align frames'
    write (*, hlp_fmt) '-ref FILENAME', 'align to this frame rather than first frame'
    write (*, hlp_fmt) '-resample [FACTOR]', 'resample before stacking (only with -align)'
    write (*, hlp_fmtc) 'default resampling factor = 1.5x'
    write (*, hlp_fmt) '-norm[alize]', 'normalize to average before stacking'
    write (*, hlp_fmt) '-nostack', 'process but do not stack images'
    write (*, hlp_fmt) '-suffix/-S FILENAME', 'suffix that will be added to file names'
    write (*, hlp_fmtc) 'when using -nostack (default: R)'
    write (*, hlp_fmt) '-temperature/-T TEMP', 'stack only frames with given CCD temperature'
    write (*, hlp_fmt) '-bias FILENAME', 'subtract this master bias'
    write (*, hlp_fmt) '-flat FILENAME', 'remove this master flat'
  end subroutine print_help

  !----------------------------------------------------------------------------!

end program
