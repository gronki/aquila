module framehandling

  use iso_fortran_env, only: real32, real64
  use ieee_arithmetic
  use globals

  !----------------------------------------------------------------------------!

  implicit none

  !----------------------------------------------------------------------------!

  type :: frame_t
    real(fp), pointer :: data(:,:) => null()
    logical, private :: auto_allocated = .false.
  contains
    procedure :: read_fits => frame_read
    procedure :: write_fits => frame_write
    procedure :: alloc_zeros => frame_alloc_zeros
    procedure :: frame_repr
    generic :: write(formatted) => frame_repr
    final :: frame_finalize
  end type

  !----------------------------------------------------------------------------!

  type, extends(frame_t) :: image_frame_t
    real(fp) :: exptime = 0, ccdtemp
    character(len = 16) :: frametyp = ""
    character(len = 256) :: fn = ""
  contains
    procedure :: read_fits => image_frame_read
    procedure :: write_fits => image_frame_write
    ! procedure :: image_frame_repr
    ! generic :: write(formatted) => image_frame_repr
  end type image_frame_t

  !----------------------------------------------------------------------------!

  ! interface image_frame_t
  !   module procedure :: image_frame_ctor_fromfile
  !   module procedure :: image_frame_ctor_zeros
  ! end interface image_frame_t

  !----------------------------------------------------------------------------!

contains

  ! !----------------------------------------------------------------------------!
  !
  ! function image_frame_ctor_fromfile(fn) result(self)
  !   type(image_frame_t) :: self
  !   character(len = *) :: fn
  !   call self % read_fits(fn)
  ! end function
  !
  ! !----------------------------------------------------------------------------!
  !
  ! function image_frame_ctor_zeros(nx, ny) result(self)
  !   type(image_frame_t) :: self
  !   integer :: nx, ny
  !   allocate(self % data(nx, ny))
  !   self % data(:,:) = 0
  ! end function

  !----------------------------------------------------------------------------!

  ! subroutine image_frame_repr(self, u, iotype, vlist, iostat, iomsg)
  !   class(image_frame_t), intent(in) :: self
  !   integer, intent(in)         :: u
  !   character(*), intent(in)    :: iotype
  !   integer, intent(in)         :: vlist(:)
  !   integer, intent(out)        :: iostat
  !   character(*), intent(inout) :: iomsg
  !
  !   if (.not. associated(self % data)) then
  !     write (u, '(a)', iostat = iostat, iomsg = iomsg) '<unallocated>'
  !   else
  !     write (u, '("<", a, " frame ", i0, "x", i0, ", ", i0, " bit, exptime = ", f10.4, ">")', &
  !       iostat = iostat, iomsg = iomsg) self % fn, shape(self % data), &
  !       storage_size(self % data), self % exptime
  !   end if
  ! end subroutine

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

  subroutine image_frame_read(self, fn, errno)
    class(image_frame_t) :: self
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
        error stop "error opening FITS file: " // trim(fn)
      end if
    end if

    ! get number of dimensions
    call ftgidm(un, ndim, ftiostat)
    if (ndim /= 2) error stop "only monochrome images are supported for now"

    ! get image dimensions
    call ftgisz(un, 2, sz, ftiostat)

    if ( .not. associated(self % data) ) then
      self % auto_allocated = .true.
      allocate(self % data(sz(1), sz(2)))
    else
      if (any(shape(self % data) /= sz)) then
        error stop "image size does not match the buffer!"
      end if
    end if

    ! read image data
    select case (storage_size(self % data))
    case (32)
      call ftgpve(un, 1, 1, product(sz), 0, self % data, anyf, ftiostat)
    case (64)
      call ftgpvd(un, 1, 1, product(sz), 0, self % data, anyf, ftiostat)
    case default
      error stop
    end select

    if (anyf) write (0, '("warning: anyf in ", a)') trim(fn)

    ! get a few keywords
    call ftgkye(un, ccdtemp_key, self % ccdtemp, "", ftiostat)
    if (ftiostat /= 0) self % ccdtemp = ieee_value(self % ccdtemp, ieee_quiet_nan)

    call ftgkye(un, exptime_key, self % exptime, "", ftiostat)
    if (ftiostat /= 0) self % exptime = ieee_value(self % exptime, ieee_quiet_nan)

    call ftgkys(un, frametype_key, self % frametyp, "", ftiostat)
    if (ftiostat /= 0) self % frametyp = ""

    ! close the unit
    ftiostat = 0
    call ftclos(un, ftiostat)
    call ftfiou(un, ftiostat)

    self % fn = fn

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error reading file: " // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine image_frame_write(self, fn, errno)
    class(image_frame_t) :: self
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
        error stop "could not create output file: " // trim(fn)
      end if
    end if

    call ftphps(un, -32, 2, shape(self % data), ftiostat)
    call ftppre(un, 1, 1, size(self % data), real(self % data, real32), ftiostat)

    if (ieee_is_normal(self % exptime)) &
    &   call ftpkyf(un, exptime_key, self % exptime, 3, "", ftiostat)

    if (ieee_is_normal(self % ccdtemp)) &
    &   call ftpkyf(un, ccdtemp_key, self % ccdtemp, 2, "", ftiostat)

    if (self % frametyp /= "") &
    &   call ftpkys(un, ccdtemp_key, self % frametyp, 2, "", ftiostat)

    call ftclos(un, ftiostat)
    call ftfiou(un, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error writing FITS file: " // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine frame_read(self, fn, errno)
    class(frame_t) :: self
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
        error stop "error opening FITS file: " // trim(fn)
      end if
    end if

    ! get number of dimensions
    call ftgidm(un, ndim, ftiostat)
    if (ndim /= 2) error stop "only monochrome images are supported for now"

    ! get image dimensions
    call ftgisz(un, 2, sz, ftiostat)

    if ( .not. associated(self % data) ) then
      allocate(self % data(sz(1), sz(2)))
      self % auto_allocated = .true.
    else
      if (any(shape(self % data) /= sz)) then
        error stop "image size does not match the buffer!"
      end if
    end if

    ! read image data
    select case (storage_size(self % data))
    case (32)
      call ftgpve(un, 1, 1, product(sz), 0, self % data, anyf, ftiostat)
    case (64)
      call ftgpvd(un, 1, 1, product(sz), 0, self % data, anyf, ftiostat)
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
        error stop "error reading file: " // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine frame_write(self, fn, errno)
    class(frame_t) :: self
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
        error stop "could not create output file: " // trim(fn)
      end if
    end if

    call ftphps(un, -32, 2, shape(self % data), ftiostat)
    call ftppre(un, 1, 1, size(self % data), real(self % data, real32), ftiostat)

    call ftclos(un, ftiostat)
    call ftfiou(un, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error writing FITS file: " // trim(fn)
      end if
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine frame_repr(self, u, iotype, vlist, iostat, iomsg)
    class(frame_t), intent(in) :: self
    integer, intent(in)         :: u
    character(*), intent(in)    :: iotype
    integer, intent(in)         :: vlist(:)
    integer, intent(out)        :: iostat
    character(*), intent(inout) :: iomsg

    if (.not. associated(self % data)) then
      write (u, '(a)', iostat = iostat, iomsg = iomsg) '<unallocated>'
    else
      write (u, '("<frame ", i0, "x", i0, ", ", i0, " bit>")', &
        iostat = iostat, iomsg = iomsg) shape(self % data), &
        storage_size(self % data)
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine frame_alloc_zeros(self, ref)
    class(frame_t), intent(inout) :: self
    class(frame_t), intent(in) :: ref

    if (.not. associated(ref % data)) error stop

    if (self % auto_allocated) then
      if (associated(self % data)) then
        deallocate(self % data)
      end if
    else
      if (associated(self % data)) error stop
    end if

    allocate(self % data(size(ref % data, 1), size(ref % data, 2)))
    self % auto_allocated = .true.
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine frame_finalize(self)
    type(frame_t) :: self
    if (self % auto_allocated .and. associated(self % data)) then
      print '("finalizing ", dt)', self
      deallocate(self % data)
    end if
  end subroutine

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
