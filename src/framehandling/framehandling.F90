module framehandling

  use iso_fortran_env, only: real32, real64
  use ieee_arithmetic
  use globals
  use str_utils_m

  use frame_m
  use image_frame_m

  !----------------------------------------------------------------------------!

  implicit none
  public

contains

  !----------------------------------------------------------------------------!

  subroutine read_fits_naxes(fn, ni, nj, errno)
    character(len = *), intent(in) :: fn
    integer, intent(out) :: ni, nj
    integer, intent(inout), optional :: errno
    integer :: sz(2), ndim, bsize, ftiostat, un
    logical :: anyf

    ftiostat = 0

    call ftgiou(un, ftiostat)
    call ftdkopn(un, fn, 0, bsize, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error opening FITS file: " // trim(fn)
      end if
    end if

    ! get number of dimensions
    call ftgidm(un, ndim, ftiostat)
    if (ndim /= 2) error stop "only monochrome images are supported for now"

    ! get image dimensions
    call ftgisz(un, 2, sz, ftiostat)
    ni = sz(2)
    nj = sz(1)

    ! close the unit
    call ftclos(un, ftiostat)
    call ftfiou(un, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error reading FITS file: " // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

end module framehandling
