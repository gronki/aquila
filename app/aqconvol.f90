program aqconvol

  use globals
  use convolutions
  use deconvolutions
  use framehandling
  use kernels

  implicit none

  real(fp), allocatable :: kernel(:,:)
  integer :: deconv_iter = -1

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
      case ("-kernel")
        if (allocated(kernel)) error stop "kernel already defined"
        if (.not. command_argument_mask(i+1)) &
          error stop "kernel type or size expected after " // trim(arg(1))
        call get_command_argument(i + 1, arg(2))
        select case (arg(2))
        case ('b3x3', 'B3x3', 'b3x3_1', 'B3x3_1')
          kernel = krn_bl3_1
        case ('b3x3_2', 'B3x3_2')
          kernel = krn_bl3_2
        case ('b3x3_3', 'B3x3_3')
          kernel = krn_bl3_3
        case default
          block
            real(fp) :: ksize
            read (arg(2), *) ksize
            print '("generating kernel FWHM = ", f8.3)', ksize
            kernel = gausskrn_alloc(ksize)
          end block
        end select

      case ('-deconv', '-deconvolution')
        if (.not. command_argument_mask(i+1)) &
          error stop "number of iterations expected after " // trim(arg(1))
        call get_command_argument(i + 1, arg(2))
        read (arg(2), *) deconv_iter
        if (deconv_iter < 1) error stop "number of iterations must be at least 1"

      case ("-h", "-help")
        call print_help(); stop
      case default
        if (arg(1)(1:1) == '-') error stop "unknown option: " // trim(arg(1))
      end select
    end do
  end block parse_cli

  if (size(kernel,2) <= 9) call print_kernel(kernel)

contains

  subroutine print_help
    write (*, '(a)') 'doopa'
  end subroutine

  

end program aqconvol
