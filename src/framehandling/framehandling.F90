module framehandling

  use iso_fortran_env, only: real32, real64
  use ieee_arithmetic
  use globals

  use frame_m
  use image_frame_m

  !----------------------------------------------------------------------------!

  implicit none
  public

contains

  !----------------------------------------------------------------------------!

  subroutine read_fits_naxes(fn, nx, ny, errno)
    character(len = *), intent(in) :: fn
    integer, intent(out) :: nx, ny
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
    nx = sz(1)
    ny = sz(2)

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

  function add_suffix(fn, suff) result(fn_out)
    character(len = *), intent(in) :: fn, suff
    character(len = :), allocatable :: fn_out
    integer :: idot

    idot = index(fn, '.', back = .True.)
    if (idot == 0) then
      fn_out = trim(fn) // suff
    else if (idot == 1) then
      error stop "this filename is incorrect"
    else
      fn_out = fn(1:idot-1) // trim(suff) // fn(idot:)
    end if
  end function

  !----------------------------------------------------------------------------!

  function replace_extn(fn, suff) result(fn_out)
    character(len = *), intent(in) :: fn, suff
    character(len = :), allocatable :: fn_out
    integer :: idot

    idot = index(fn, '.', back = .True.)
    if (idot == 0) then
      fn_out = trim(fn) // suff
    else
      fn_out = fn(1:idot) // suff
    end if
  end function

  !----------------------------------------------------------------------------!

  elemental logical function endswith(buf, suff)
    character(len = *), intent(in) :: buf, suff
    integer :: n
    if (len_trim(buf) >= len_trim(suff)) then
      n = len_trim(buf)
      endswith = buf(n - len_trim(suff) + 1 : n) == trim(suff)
    else
      endswith = .false.
    end if
  end function

  !----------------------------------------------------------------------------!

end module framehandling
