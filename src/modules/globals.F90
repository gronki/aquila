module globals

  use iso_fortran_env, only: stderr => error_unit, stdout => output_unit

  implicit none

  integer, parameter :: fp = selected_real_kind(12)

  logical :: cfg_verbose = .false.
  character(len = *), parameter :: version = '190802'

contains

  subroutine greeting(progname)
    use iso_fortran_env, only: compiler_options, compiler_version
    character(len = *), intent(in) :: progname
    write (*, '(a," v.",a)') trim(progname), version
    write (*, '("built with ",a)') compiler_version()
    write (*,*)
  end subroutine

  pure function cf(s, f)
    character(len = *), intent(in) :: s, f
    character(len = :), allocatable :: cf
#   ifdef _WIN32
    cf = trim(s)
#   else
    cf = achar(27) // '[' // trim(f) // 'm' // trim(s) // achar(27) // '[0m'
#   endif
  end function

end module
