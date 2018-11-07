program aqlrgb

  use globals

  implicit none

  write (*, '(10x,a)') '*** AQUILA v.' // version // ' ***'

  if (command_argument_count() == 0) then
    call print_help(); stop
  end if

  parse_cli: block
    integer :: i
    character(len = 256) :: arg, buf
    logical :: skip = .false.

    do i = 1, command_argument_count()
      if (skip) then
        skip = .false.; cycle
      end if
      call get_command_argument(i, arg)
      select case (arg)
      case ('-o', '-O', '-output')
        call get_command_argument(i + 1, buf)
        if (buf == "" .or. buf(1:1) == '-') error stop "file name expected"
        outfn = buf
        skip = .true.
      case ("-h", "-help")
        call print_help(); stop
      case default
        if (arg(1:1) == '-') error stop "unknown option: " // trim(arg)
      end select
    end do
  end block parse_cli

contains

  subroutine print_help()
    character(len = *), parameter :: fmt = '(a28, 2x, a)', fmt_ctd = '(30x, a)'
    write (*, fmt) '-h[elp]', 'prints help'
  end subroutine

end program aqlrgb
