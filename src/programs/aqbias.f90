program aqbias

  !----------------------------------------------------------------------------!

  use globals

  !----------------------------------------------------------------------------!

  implicit none

  ! allowed values: average, median, sigclip
  character(len = 16) :: bias_method
  character(len = 256) :: output_fn
  character(len = 256), allocatable :: input_fn(:)

  integer :: nframes

  !----------------------------------------------------------------------------!

  bias_method = ""
  output_fn = ""
  allocate(input_fn(0))

  !----------------------------------------------------------------------------!

  if (command_argument_count() == 0) then
    print_help: block
      character(len = *), parameter :: fmt = '(a28, 2x, a)'
      write (stdout, '(a)') 'usage: aqbias [options] -o [output] [file1] [file2] ...'
      write (stdout, fmt) '-o/-output [filename]', 'specifies the output filename'
      write (stdout, fmt) '-av/-average', 'compute bias by average value'
      write (stdout, fmt) '-md/-median', 'compute bias by median value'
      stop
    end block print_help
  end if

  !----------------------------------------------------------------------------!

  parse_cli: block
    integer :: i
    character(len = 256) :: arg
    logical, allocatable :: command_argument_mask(:)

    allocate(command_argument_mask(command_argument_count()))
    command_argument_mask(:) = .true.

    scan_cli: do i = 1, command_argument_count()

      if (.not. command_argument_mask(i)) cycle scan_cli
      call get_command_argument(i, arg)

      select case (arg)

      case ("-md", "-median")
        if (bias_method /= "") &
          error stop "method already set to " // trim(bias_method)
        bias_method = "median"
        command_argument_mask(i) = .false.

      case ("-av", "-average")
        if (bias_method /= "") &
          error stop "method already set to " // trim(bias_method)
        bias_method = "average"
        command_argument_mask(i) = .false.

      case ("-o", "-output")
        if (output_fn /= "") &
          error stop "output filename has been defined twice"

        if (i == command_argument_count()) then
          error stop "output file name expected"
        else
          call get_command_argument(i+1, output_fn)
          if (.not. command_argument_mask(i+1)) then
            error stop "output file name expected, got: " // trim(output_fn)
          end if
          command_argument_mask(i:i+1) = .false.
        end if

      case default
        if (arg(1:1) == '-') error stop "unknown option " // trim(arg)

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

  if (size(input_fn) == 0) error stop "no input files!"
  nframes = size(input_fn)

  if (bias_method == "")  bias_method = "average"
  if (output_fn == "")    output_fn = "out.fits"

  verify_cli_arguments: block
    character(len = *), parameter :: fmt = '(a12, ": ", g0)'
    integer :: i

    do i = 1, nframes
      write (stderr, fmt) 'input', trim(input_fn(i))
    end do
    write (stderr, fmt) 'frames', nframes
    write (stderr, fmt) 'method', trim(bias_method)
    write (stderr, fmt) 'output', trim(output_fn)
  end block verify_cli_arguments

  !----------------------------------------------------------------------------!

  if (bias_method /= "average") error stop "methods other than average&
      & are not yet implemented"

  !----------------------------------------------------------------------------!

  
  !----------------------------------------------------------------------------!

end program aqbias
