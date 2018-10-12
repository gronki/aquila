program aqstack

  !----------------------------------------------------------------------------!

  use globals
  use framehandling
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
  logical :: cfg_estimate_noise = .false.

  integer :: nframes

  !----------------------------------------------------------------------------!

  allocate(input_fn(0))

  !----------------------------------------------------------------------------!

  if (command_argument_count() == 0) then
    call print_help()
    stop
  end if

  !----------------------------------------------------------------------------!

  parse_cli: block
    integer :: i, nargs
    character(len = 256) :: arg
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

      case ("-mdn", "-median")
        method = "median"
        command_argument_mask(i) = .false.

      case ("-avg", "-average")
        method = "average"
        command_argument_mask(i) = .false.

      case ("-noise", "-estimate-noise")
        cfg_estimate_noise = .true.
        command_argument_mask(i) = .false.

      case ("-process", "-process-only", "-nostack")
        cfg_process_only = .true.
        command_argument_mask(i) = .false.

      case ("-al", "-align")
        cfg_align_frames = .true.
        command_argument_mask(i) = .false.

      case ("-o", "-O", "-output")
        if (output_fn /= "") &
          error stop "output filename has been defined twice"
        if (.not. command_argument_mask(i + 1)) then
          error stop "output file name expected"
        else
          call get_command_argument(i + 1, output_fn)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-addsuffix", "-suffix", "-suff")
        if (output_suff /= "") &
          error stop "suffix has been defined twice"
        if (.not. command_argument_mask(i + 1)) then
          error stop "suffix expected"
        else
          call get_command_argument(i + 1, output_suff)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-bias")
        if (bias_fn /= "") &
          error stop "bias filename has been defined twice"
        if (.not. command_argument_mask(i + 1)) then
          error stop "bias file name expected"
        else
          call get_command_argument(i + 1, bias_fn)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-flat")
        if (flat_fn /= "") &
          error stop "flat filename has been defined twice"
        if (.not. command_argument_mask(i + 1)) then
          error stop "flat file name expected"
        else
          call get_command_argument(i + 1, flat_fn)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-ref", "-align-reference")
        if (ref_fn /= "") &
          error stop "reference frame filename has been defined twice"
        if (.not. command_argument_mask(i + 1)) then
          error stop "reference frame file name expected"
        else
          call get_command_argument(i + 1, ref_fn)
          command_argument_mask(i : i + 1) = .false.
        end if

      case ("-h", "-help")
        call print_help()
        stop

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
  if (output_suff == "") output_suff = "-red"

  verify_cli_arguments: block
    character(len = *), parameter :: fmt = '(a12, ": ", g0)'
    integer :: i

    write (stderr, fmt) 'frames', nframes
    write (stderr, fmt) 'method', trim(method)
    write (stderr, fmt) 'output', trim(output_fn)
  end block verify_cli_arguments

  !----------------------------------------------------------------------------!

  if (method /= "average") error stop "methods other than average&
      & are not yet implemented"

  !----------------------------------------------------------------------------!

  actual_job: block

    type(image_frame), dimension(:), allocatable :: frames
    real(fp), allocatable, target :: buffer(:,:,:)
    integer :: i, n, errno
    integer :: nx, ny

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

    if (bias_fn /= "") then
      if (strategy == "bias") error stop "why subtract bias from bias?"
      subtract_bias: block
        integer :: i
        type(image_frame) :: frame_calib

        allocate(frame_calib % data(nx, ny))
        call frame_calib % read_fits(bias_fn)

        write (0, *) "subtracting bias..."
        do i = 1, n
          buffer(:,:,i) = buffer(:,:,i) - frame_calib % data(:,:)
        end do

        deallocate(frame_calib % data)
      end block subtract_bias
    end if

    if (flat_fn /= "") then
      if (strategy == "flat") error stop "why subtract flat from flat?"
      subtract_flat: block
        integer :: i
        type(image_frame) :: frame_calib

        allocate(frame_calib % data(nx, ny))
        call frame_calib % read_fits(bias_fn)

        frame_calib % data(:,:) = frame_calib % data &
          / (sum(frame_calib % data) / (nx * ny))

        write (0, *) "removing flat..."
        do i = 1, n
          buffer(:,:,i) = buffer(:,:,i) / frame_calib % data(:,:)
        end do

        deallocate(frame_calib % data)
      end block subtract_flat
    end if

    if (cfg_clean_cosmics) then
      write (0, '(a)') 'warning: cosmic rays clean not implemented yet'
    end if

    if (cfg_auto_invert) then
      write (0, '(a)') 'warning: auto rotation not implemented yet'
    end if

    if (cfg_align_frames .and. n > 1) then
      align_frames: block
        use legacy_align, only: align_xyr, improject
        real(fp), dimension(:,:), allocatable :: buf_copy
        type(extended_source), dimension(:), allocatable :: lst0, lst
        real(fp) :: mx(2,3)
        integer :: i

        allocate(buf_copy(nx, ny))

        call findstar_local(buffer(:,:,1), lst0)

        do i = 2, n
          call findstar_local(buffer(:,:,i), lst)
          call align_xyr(lst0, lst, mx)

          buf_copy(:,:) = buffer(:,:,i)
          buffer(:,:,i) = 0
          call improject(buf_copy, mx, buffer(:,:,i))
        end do
      end block align_frames
    end if

    if (cfg_process_only) then
      save_processed: block
        integer :: i
        character(len = 256) :: newfn
        if ( n == 1 ) then
          call frames(1) % write_fits(output_fn)
        else
          do i = 1, n
            call add_suffix(frames(i) % fn, output_suff, newfn)
            call frames(i) % write_fits(newfn)
          end do
        end if
      end block save_processed
    else
      actual_stack: block
        type(image_frame) :: frame_out
        allocate(frame_out % data(size(buffer, 1), size(buffer, 2)))
        select case (method)
        case ('average')
          frame_out % data(:,:) = sum(buffer(:,:,1:n), 3) / n
        case default
          error stop "this averaging method is not supported"
        end select

        frame_out % exptime = average_safe_1d(frames(1:n) % exptime)
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
    character(len = *), parameter :: fmt = '(a28, 2x, a)'
    write (stdout, '(a)') 'usage: aquila [STRATEGY] [OPTIONS] FILE1 [FILE2 ...] -o OUTPUT'
    write (stdout, '(a)') ' STRATEGY can be: bias, dark, flat, process, stack'
    write (stdout, fmt) '-o/-output FILENAME', 'specifies the output filename'
    write (stdout, fmt) '-avg/-average', 'compute bias by average value'
    ! write (stdout, fmt) '-mdn/-median', 'compute bias by median value (not supported)'
  end subroutine print_help

  subroutine findstar_local(im, lst)

    use convolution, only: convol_fix
    use kernels, only: mexhakrn_alloc
    use findstar, only: aqfindstar

    real(fp), intent(in) :: im(:,:)
    type(extended_source), intent(out), allocatable :: lst(:)
    real(fp), allocatable :: im2(:,:), krn(:,:)
    integer :: nstars

    krn = mexhakrn_alloc(2.2_fp)

    allocate(im2(size(im,1), size(im,2)))
    call convol_fix(im, krn, im2, 'r')
    call aqfindstar(im2, lst, limit = 150)

  end subroutine

  pure function average_safe_1d(x) result(m)
    real(fp), intent(in) :: x(:)
    real(fp) :: m
    logical :: mask(size(x))
    mask(:) = ieee_is_normal(x)
    m = sum(x, mask) / count(mask)
  end function

end program
