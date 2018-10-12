module framehandling

  use iso_fortran_env, only: real32, real64
  use ieee_arithmetic
  use globals

  !----------------------------------------------------------------------------!

  implicit none

  !----------------------------------------------------------------------------!

  type :: basic_frame
    real(fp), pointer :: data(:,:) => null()
  contains
    procedure :: read_fits => basic_frame_read
    procedure :: write_fits => basic_frame_write
  end type

  type, extends(basic_frame) :: image_frame
    real(fp) :: exptime = 0, ccdtemp
    character(len = 16) :: frametyp = ""
    character(len = 256) :: fn = ""
  contains
    procedure :: read_fits => image_frame_read
    procedure :: write_fits => image_frame_write
    procedure :: image_frame_repr
    generic :: write(formatted) => image_frame_repr
    ! final :: image_frame_finalize
  end type image_frame

  !----------------------------------------------------------------------------!

  ! interface image_frame
  !   module procedure :: image_frame_ctor_fromfile
  !   module procedure :: image_frame_ctor_zeros
  ! end interface image_frame

  !----------------------------------------------------------------------------!

contains

  ! !----------------------------------------------------------------------------!
  !
  ! function image_frame_ctor_fromfile(fn) result(imfr)
  !   type(image_frame) :: imfr
  !   character(len = *) :: fn
  !   call imfr % read_fits(fn)
  ! end function
  !
  ! !----------------------------------------------------------------------------!
  !
  ! function image_frame_ctor_zeros(nx, ny) result(imfr)
  !   type(image_frame) :: imfr
  !   integer :: nx, ny
  !   allocate(imfr % data(nx, ny))
  !   imfr % data(:,:) = 0
  ! end function

  !----------------------------------------------------------------------------!

  subroutine image_frame_repr(imfr, u, iotype, vlist, iostat, iomsg)
    class(image_frame), intent(in) :: imfr
    integer, intent(in)         :: u
    character(*), intent(in)    :: iotype
    integer, intent(in)         :: vlist(:)
    integer, intent(out)        :: iostat
    character(*), intent(inout) :: iomsg

    if (.not. associated(imfr % data)) then
      write (u, '(a)', iostat = iostat, iomsg = iomsg) '<unallocated>'
    else
      write (u, '("<frame ", i0, "x", i0, ", ", i0, " bit, exptime = ", f10.4, ">")', &
        iostat = iostat, iomsg = iomsg) shape(imfr % data),  storage_size(imfr % data), imfr % exptime
    end if
  end subroutine

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
        error stop "error opening FITS file" // trim(fn)
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
        error stop "error reading FITS file" // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine image_frame_read(imfr, fn, errno)
    class(image_frame) :: imfr
    character(len = *), intent(in) :: fn
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
        error stop "error opening FITS file" // trim(fn)
      end if
    end if

    ! get number of dimensions
    call ftgidm(un, ndim, ftiostat)
    if (ndim /= 2) error stop "only monochrome images are supported for now"

    ! get image dimensions
    call ftgisz(un, 2, sz, ftiostat)

    if ( .not. associated(imfr % data) ) then
      allocate(imfr % data(sz(1), sz(2)))
    else
      if (any(shape(imfr % data) /= sz)) then
        error stop "image size does not match the buffer!"
      end if
    end if

    ! read image data
    select case (storage_size(imfr % data))
    case (32)
      call ftgpve(un, 1, 1, product(sz), 0, imfr % data, anyf, ftiostat)
    case (64)
      call ftgpvd(un, 1, 1, product(sz), 0, imfr % data, anyf, ftiostat)
    case default
      error stop
    end select

    if (anyf) write (0, '("warning: anyf in ", a)') trim(fn)

    ! get a few keywords
    call ftgkye(un, ccdtemp_key, imfr % ccdtemp, "", ftiostat)
    if (ftiostat /= 0) imfr % ccdtemp = ieee_value(imfr % ccdtemp, ieee_quiet_nan)

    call ftgkye(un, exptime_key, imfr % exptime, "", ftiostat)
    if (ftiostat /= 0) imfr % exptime = ieee_value(imfr % exptime, ieee_quiet_nan)

    call ftgkys(un, frametype_key, imfr % frametyp, "", ftiostat)
    if (ftiostat /= 0) imfr % frametyp = ""

    ! close the unit
    ftiostat = 0
    call ftclos(un, ftiostat)
    call ftfiou(un, ftiostat)

    imfr % fn = fn

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error reading file" // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine image_frame_write(imfr, fn, errno)
    class(image_frame) :: imfr
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno
    integer :: ftiostat, un

    ftiostat = 0

    call ftgiou(un, ftiostat)
    call ftdkinit(un, fn, 1, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "could not create output file" // trim(fn)
      end if
    end if

    call ftphps(un, -32, 2, shape(imfr % data), ftiostat)
    call ftppre(un, 1, 1, size(imfr % data), real(imfr % data, real32), ftiostat)

    if (ieee_is_normal(imfr % exptime)) &
    &   call ftpkyf(un, exptime_key, imfr % exptime, 3, "", ftiostat)

    if (ieee_is_normal(imfr % ccdtemp)) &
    &   call ftpkyf(un, ccdtemp_key, imfr % ccdtemp, 2, "", ftiostat)

    if (imfr % frametyp /= "") &
    &   call ftpkys(un, ccdtemp_key, imfr % frametyp, 2, "", ftiostat)

    call ftclos(un, ftiostat)
    call ftfiou(un, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error writing FITS file" // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine basic_frame_read(imfr, fn, errno)
    class(basic_frame) :: imfr
    character(len = *), intent(in) :: fn
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
        error stop "error opening FITS file" // trim(fn)
      end if
    end if

    ! get number of dimensions
    call ftgidm(un, ndim, ftiostat)
    if (ndim /= 2) error stop "only monochrome images are supported for now"

    ! get image dimensions
    call ftgisz(un, 2, sz, ftiostat)

    if ( .not. associated(imfr % data) ) then
      allocate(imfr % data(sz(1), sz(2)))
    else
      if (any(shape(imfr % data) /= sz)) then
        error stop "image size does not match the buffer!"
      end if
    end if

    ! read image data
    select case (storage_size(imfr % data))
    case (32)
      call ftgpve(un, 1, 1, product(sz), 0, imfr % data, anyf, ftiostat)
    case (64)
      call ftgpvd(un, 1, 1, product(sz), 0, imfr % data, anyf, ftiostat)
    case default
      error stop
    end select

    if (anyf) write (0, '("warning: anyf in ", a)') trim(fn)

    ! close the unit
    call ftclos(un, ftiostat)
    call ftfiou(un, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error reading file" ! // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine basic_frame_write(imfr, fn, errno)
    class(basic_frame) :: imfr
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno
    integer :: ftiostat, un

    ftiostat = 0

    call ftgiou(un, ftiostat)
    call ftdkinit(un, fn, 1, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "could not create output file" // trim(fn)
      end if
    end if

    call ftphps(un, -32, 2, shape(imfr % data), ftiostat)
    call ftppre(un, 1, 1, size(imfr % data), real(imfr % data, real32), ftiostat)

    call ftclos(un, ftiostat)
    call ftfiou(un, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error writing FITS file" // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  ! subroutine image_frame_finalize(imfr)
  !   type(image_frame) :: imfr
  !   print '("finalizing ", dt)', imfr
  !   if (allocated(imfr % data)) deallocate(imfr % data)
  ! end subroutine

  !----------------------------------------------------------------------------!

  subroutine add_suffix(fn, suff, fn_out)
    character(len = *), intent(in) :: fn, suff
    character(len = *), intent(out) :: fn_out
    character(len = *), parameter :: ext = "fits"
    integer :: idot

    idot = index(fn, '.', back = .True.)
    if (idot == 0) then
      fn_out = trim(fn) // suff
    else if (idot == 1) then
      error stop "this filename is incorrect"
    else
      fn_out = fn(1:idot-1) // trim(suff) // fn(idot:)
    end if
  end subroutine

  !----------------------------------------------------------------------------!

end module framehandling
