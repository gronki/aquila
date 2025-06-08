module frame_m

  use iso_fortran_env, only: real32, real64
  use ieee_arithmetic
  use globals

  !----------------------------------------------------------------------------!

  implicit none
  private

  !----------------------------------------------------------------------------!

  type, public :: frame_t
    real(fp), allocatable :: data(:,:)
  contains
    procedure :: read_fits, write_fits, check_shape
    procedure, private :: read_image_data
    ! procedure :: assign_data, assign_frame
    ! generic :: copy => assign_data, assign_frame
    procedure, private :: repr
    generic :: write(formatted) => repr
  end type

  interface frame_t
    module procedure :: frame_t_ctor
    module procedure :: frame_t_ctor_buf
    module procedure :: frame_t_ctor_file
  end interface

contains

  !----------------------------------------------------------------------------!

  function frame_t_ctor() result(fr)
    type(frame_t):: fr
  end function

  !----------------------------------------------------------------------------!

  function frame_t_ctor_file(file) result(fr)
    type(frame_t):: fr
    character(len=*), intent(in) :: file

    call fr % read_fits(file)
  end function

  !----------------------------------------------------------------------------!

  function frame_t_ctor_buf(buf) result(fr)
    type(frame_t):: fr
    real(fp), intent(in) :: buf(:,:)

    fr % data = buf
  end function

  !----------------------------------------------------------------------------!

  pure subroutine check_shape(self, nx, ny)
    class(frame_t), intent(inout) :: self
    integer, intent(in) :: nx, ny

    if (allocated(self % data)) then
      if (size(self%data, 1) == nx .and. size(self%data, 2) == ny) return
      deallocate(self % data)
    end if

    allocate(self%data(nx, ny))
    
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine read_image_data(self, un, ftiostat)
    class(frame_t), intent(inout) :: self
    integer :: sz(2), ndim, bsize, ftiostat, un
    logical :: anyf
    ! get number of dimensions
    call ftgidm(un, ndim, ftiostat)
    if (ndim /= 2) error stop "only monochrome images are supported for now"

    ! get image dimensions
    call ftgisz(un, 2, sz, ftiostat)
    call self % check_shape(sz(1), sz(2))

    ! read image data
    select case (storage_size(self % data))
    case (32)
      call ftgpve(un, 1, 1, product(sz), 0, self % data, anyf, ftiostat)
    case (64)
      call ftgpvd(un, 1, 1, product(sz), 0, self % data, anyf, ftiostat)
    case default
      error stop
    end select
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine read_fits(self, fn, errno)
    class(frame_t), intent(inout) :: self
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno
    integer :: bsize, ftiostat, un

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

    call self % read_image_data(un, ftiostat)

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

  subroutine write_fits(self, fn, errno)
    class(frame_t) :: self
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno
    integer :: ftiostat, un, iostat

    iostat = 0
    open (99, file = fn, status = 'old', iostat = iostat)
    if (iostat == 0) then
      write (0, '("file ",a," exists, deleting...")') trim(fn)
      close (99, status = 'delete')
    end if

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

  subroutine repr(self, u, iotype, vlist, iostat, iomsg)
    class(frame_t), intent(in) :: self
    integer, intent(in)         :: u
    character(*), intent(in)    :: iotype
    integer, intent(in)         :: vlist(:)
    integer, intent(out)        :: iostat
    character(*), intent(inout) :: iomsg

    if (.not. allocated(self % data)) then
      write (u, '(a)', iostat=iostat, iomsg=iomsg) 'frame_t(unallocated)'
    else
      write (u, '("frame_t(", i0, ",", i0, ")")', &
        iostat=iostat, iomsg=iomsg) shape(self % data)
    end if
  end subroutine

  !----------------------------------------------------------------------------!

end module