module globals

  use iso_c_binding
  use iso_fortran_env, stderr => error_unit, stdout => output_unit

  implicit none
# ifdef SINGLE
  integer, parameter :: fp = c_float
# else
  integer, parameter :: fp = c_double
# endif

  character(len = *), parameter :: hlp_fmt = '(a25, 2x, a)', hlp_fmtc = '(27x, a)'
  character(len = *), parameter :: fmthlp = '(a25, 2x, a, :/, *(27x, a, :/))'
# include "../version.h"
  character(len = *), parameter :: version = _AQUILA_VERSION_

  character(len = *), parameter :: perf_fmt = '("PERF", a10, 1x, "=", f7.3)'

  logical :: cfg_verbose = .false.

contains

  subroutine greeting(progname)
    character(len = *), intent(in) :: progname
    print '(*(a))', trim(progname), ' v.', version, ' (https://github.com/gronki/aquila)'
  end subroutine

  pure function cf(s, f)
    character(len = *), intent(in) :: s
    character(len = *), intent(in), optional :: f
    character(len = :), allocatable :: cf
#   ifdef _WIN32
#   define _NOCOLOR
#   endif
#   ifdef _NOCOLOR
    cf = trim(s)
#   else
    if ( present(f) ) then
      cf = achar(27) // '[' // trim(f) // 'm' // trim(s) // achar(27) // '[0m'
    else
      cf = achar(27) // '[' // '1'     // 'm' // trim(s) // achar(27) // '[0m'
    end if
#   endif
  end function

end module
