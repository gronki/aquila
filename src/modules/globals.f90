module globals

  use iso_fortran_env, only: stderr => error_unit, stdout => output_unit

  implicit none

  integer, parameter :: fp = selected_real_kind(12)

  logical :: cfg_verbose = .false.
  character(len = *), parameter :: version = '190802'

contains

  subroutine greeting(progname)
    character(len = *), intent(in) :: progname
    write (*, '(6x,a)') '*** '// achar(27) //'[1m'// trim(progname) &
    & // achar(27) //'[0m v.' // version // ' ***'
  end subroutine

  pure function cf(s, f)
    character(len = *), intent(in) :: s, f
    character(len = :), allocatable :: cf
    cf = achar(27) // '[' // trim(f) // 'm' // trim(s) // achar(27) // '[0m'
  end function

end module
