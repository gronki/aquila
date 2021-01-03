module globals

  use iso_fortran_env, stderr => error_unit, stdout => output_unit

  implicit none

  integer, parameter :: fp = real64

  character(len = *), parameter :: hlp_fmt = '(a26, 2x, a)', hlp_fmtc = '(28x, a)'
  character(len = *), parameter :: version = '210103'

  character(len = *), parameter :: perf_fmt = '("PERF", a10, 1x, "=", f7.3)'

  logical :: cfg_verbose = .false.

contains

  subroutine greeting(progname)
    use iso_fortran_env, only: compiler_options, compiler_version
    character(len = *), intent(in) :: progname
    write (*, '(a," v.",a)') trim(progname), version
    write (*, '("built with ",a)') compiler_version()
    write (*,*)
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
