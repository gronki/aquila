module globals

  use iso_fortran_env, only: stderr => error_unit, stdout => output_unit

  implicit none

  integer, parameter :: fp = selected_real_kind(12)

  logical :: cfg_verbose = .false.
  character(len = *), parameter :: ccdtemp_key = 'CCD-TEMP'
  character(len = *), parameter :: frametype_key = 'FRAME'
  character(len = *), parameter :: exptime_key = 'EXPTIME'

  character(len = *), parameter :: version = '181106'

contains

  subroutine greeting(progname)
    character(len = *), intent(in) :: progname
    write (*, '(6x,a)') '*** '// achar(27) //'[1m'// trim(progname) &
    & // achar(27) //'[0m v.' // version // ' ***'
  end subroutine

end module
