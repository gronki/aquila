program aqlrgb

  use globals

  implicit none

  write (*, '(10x,a)') '*** AQUILA v.' // version // ' ***'

  if (command_argument_count() == 0) then
    call print_help(); stop
  end if

  parse_cli: block
    integer :: i, nargs
    character(len = 256) :: arg(3)
    logical, allocatable :: command_argument_mask(:)

    nargs = command_argument_count()
    allocate(command_argument_mask(nargs + 2))
    command_argument_mask(:nargs) = .true.
    command_argument_mask(nargs+1:) = .false.

    do i = 1, nargs
      if (.not. command_argument_mask(i)) cycle
      call get_command_argument(i, arg(1))

      select case (arg(1))
      case ("-h", "-help")
        call print_help(); stop
      case default
        if (arg(1)(1:1) == '-') error stop "unknown option: " // trim(arg(1))
      end select
    end do
  end block parse_cli

contains

  subroutine print_help()
  end subroutine

end program aqlrgb
