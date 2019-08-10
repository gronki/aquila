module frame_m

  use iso_fortran_env, only: real32, real64
  use ieee_arithmetic
  use globals

  !----------------------------------------------------------------------------!

  implicit none
  private

  !----------------------------------------------------------------------------!

  type, public :: frame_t
    real(fp), pointer, contiguous :: data(:,:) => null()
    logical, private :: auto_allocated = .false.
  contains
    procedure :: read_fits, write_fits, alloc_zeros
    procedure :: assign_2d, assign_frame, assign_const_f, assign_const_i
    generic :: assignment(=) => assign_2d, assign_frame, &
    &   assign_const_f, assign_const_i
    procedure :: repr
    generic :: write(formatted) => repr
    final :: finalize
  end type

contains

  !----------------------------------------------------------------------------!

  subroutine assign_frame(fr, ref)
    class(frame_t), intent(inout) :: fr
    class(frame_t), intent(in) :: ref
    if (.not. associated(ref % data)) error stop
    call assign_2d(fr, ref % data)
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine assign_2d(fr, im)
    class(frame_t), intent(inout) :: fr
    real(fp), dimension(:,:), intent(in) :: im
    ! if the buffer is there, try to use it
    if (associated(fr % data)) then
      ! if sizes don't match, we need to reasllocate it
      if (any(shape(fr % data) /= shape(im))) then
        ! for manual buffers we don't touch them
        if (.not. fr % auto_allocated) then
          error stop "this frame buffer was manually assigned"
        end if
        ! auto buffers can be resized
        deallocate(fr % data)
        allocate(fr % data(size(im, 1), size(im, 2)))
      end if
    else
      ! buffer was not allocated
      allocate(fr % data(size(im, 1), size(im, 2)))
      fr % auto_allocated = .true.
    end if
    ! at this point we have the matching buffer so we can do the copy
    fr % data(:,:) = im(:,:)
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine assign_const_f(fr, c)
    class(frame_t), intent(inout) :: fr
    real(fp), intent(in) :: c
    if (associated(fr % data)) then
      fr % data(:,:) = c
    end if
  end subroutine

  subroutine assign_const_i(fr, c)
    class(frame_t), intent(inout) :: fr
    integer, intent(in) :: c
    if (associated(fr % data)) then
      fr % data(:,:) = c
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine read_fits(self, fn, errno)
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

  subroutine write_fits(self, fn, errno)
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

  subroutine repr(self, u, iotype, vlist, iostat, iomsg)
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

  subroutine alloc_zeros(self, ref)
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

  subroutine finalize(self)
    type(frame_t) :: self
    if (self % auto_allocated .and. associated(self % data)) then
      print '("' // cf('finalizing','91') // '", 1x, dt)', self
      deallocate(self % data)
    end if
  end subroutine

  !----------------------------------------------------------------------------!

end module

!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!

module fitsheader_m

  use globals

  !----------------------------------------------------------------------------!

  implicit none
  private

  !----------------------------------------------------------------------------!

  integer, parameter :: kvlen = 128

  type fhentry
    character(len = kvlen) :: key, value
    class(fhentry), pointer :: next => null()
  end type

  type, public :: fhdict
    type(fhentry), pointer :: list => null()
  contains
    procedure :: add_str, get_str, get_float, get_int, has_key
    procedure, private :: fhdict_repr
    generic :: write(formatted) => fhdict_repr
    procedure, pass(self), private :: has_key_op
    generic :: operator(.in.) => has_key_op
    final :: finalize
  end type

contains

  !----------------------------------------------------------------------------!

  logical function has_key(self, k) result(has)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    type(fhentry), pointer :: cur

    has = .false.

    cur => self % list
    do while (associated(cur))
      if (cur % key == k) then
        has = .true.; return
      end if
      cur => cur % next
    end do
  end function

  logical function has_key_op(k, self) result(has)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    has = self % has_key(k)
  end function

  !----------------------------------------------------------------------------!

  subroutine add_str(self, k, v)
    class(fhdict), intent(inout) :: self
    character(len = *), intent(in) :: k, v
    type(fhentry), pointer :: newentry, cur

    allocate(newentry)
    newentry % key = k
    newentry % value = v

    if ( associated(self % list) ) then
      cur => self % list
      find_tail: do
        if (.not. associated(cur % next)) then
          cur % next => newentry; exit
        end if
        cur => cur % next
      end do find_tail
    else
      self % list => newentry
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine get_str(self, k, buf, errno)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    character(len = *), intent(inout) :: buf
    integer, intent(inout), optional :: errno
    type(fhentry), pointer :: cur

    cur => self % list
    do while (associated(cur))
      if (cur % key == k) then
        buf = cur % value
        if (present(errno)) errno = 0
        return
      end if
      cur => cur % next
    end do

    ! not found
    if (present(errno)) then
      errno = -1
    else
      error stop 'key ' // trim(k) // ' not in the dict'
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  real function get_float(self, k, errno) result(v)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    integer, intent(inout), optional :: errno
    character(len = kvlen) :: buf

    call self % get_str(k, buf, errno)
    if (present(errno)) then
      if (errno /= 0) return
      read (buf, *, iostat = errno) v
    else
      read (buf, *) v
    end if
  end function

  integer function get_int(self, k, errno) result(v)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    integer, intent(inout), optional :: errno
    character(len = kvlen) :: buf

    call self % get_str(k, buf, errno)
    if (present(errno)) then
      if (errno /= 0) return
      read (buf, *, iostat = errno) v
    else
      read (buf, *) v
    end if
  end function

  !----------------------------------------------------------------------------!

  subroutine fhdict_repr(self, u, iotype, vlist, iostat, iomsg)
    class(fhdict), intent(in) :: self
    integer, intent(in)         :: u
    character(*), intent(in)    :: iotype
    integer, intent(in)         :: vlist(:)
    integer, intent(out)        :: iostat
    character(*), intent(inout) :: iomsg
    integer :: i
    type(fhentry), pointer :: cur

    if (associated(self % list)) then
      cur => self % list
      write (u, '(a)', iostat = iostat, iomsg = iomsg) '[ '
      do while (associated(cur))
        write (u, '(a, " => ", a, a)', iostat = iostat, iomsg = iomsg) &
        &   trim(cur % key), trim(cur % value), &
        &   merge(', ', ' ]', associated(cur % next))
        cur => cur % next
      end do
    else
      write (u, '(a)', iostat = iostat, iomsg = iomsg) '<empty fhdict>'
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine finalize(self)
    type(fhdict), intent(inout) :: self
    class(fhentry), pointer :: cur, nxt
    cur => self % list
    do while (associated(cur))
      nxt => cur % next
      ! write (0, *) 'fhdict: finalizing ', trim(cur % key)
      deallocate(cur)
      cur => nxt
    end do
  end subroutine finalize

end module

!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!

module image_frame_m

  use frame_m
  use fitsheader_m
  use globals

  !----------------------------------------------------------------------------!

  implicit none
  private

  !----------------------------------------------------------------------------!

  type, extends(frame_t), public :: image_frame_t
    real(fp) :: exptime = 0, ccdtemp
    character(len = 16) :: frametyp = ""
    character(len = 256) :: fn = ""
    type(fhdict) :: hdr
  contains
    procedure :: read_fits, write_fits
    procedure, private :: loadkw
    ! procedure :: image_repr
    ! generic :: write(formatted) => image_repr
  end type image_frame_t

  !----------------------------------------------------------------------------!

  ! interface image_frame_t
  !   module procedure :: image_frame_ctor_fromfile
  !   module procedure :: image_frame_ctor_zeros
  ! end interface image_frame_t

  !----------------------------------------------------------------------------!

contains

  subroutine loadkw(self, u, k, errno)
    class(image_frame_t) :: self
    integer, intent(in) :: u
    character(len = *), intent(in) :: k
    integer, intent(inout), optional :: errno

    character(len = 256) :: commt, s
    integer :: status

    status = 0
    call ftgkys(u, trim(k), s, commt, status)
    if (present(errno)) errno = status

    if (status == 0) then
      call self % hdr % add_str(k, s)
    else
      write (0, *) 'warning: no keyword ', trim(k)
    end if
  end subroutine

  subroutine read_fits(self, fn, errno)
    class(image_frame_t) :: self
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno
    integer :: un, status, bsize

    write (0, *) 'image_frame_t % read_fits'

    status = 0

    call self % frame_t % read_fits(fn, status)
    if (present(errno)) errno = status

    if (status == 0) then
      self % fn = fn

      call ftgiou(un, status)
      call ftdkopn(un, fn, 0, bsize, status)

      call self % loadkw(un, 'EXPTIME')
      if ('EXPTIME' .in. self % hdr) &
        self % exptime = self % hdr % get_float('EXPTIME')
      call self % loadkw(un, 'CCD-TEMP')
      if ('CCD-TEMP' .in. self % hdr) &
        self % ccdtemp = self % hdr % get_float('CCD-TEMP')
      call self % loadkw(un, 'FRAME')
      call self % loadkw(un, 'DATE-OBS')

      call ftclos(un, status)
      call ftfiou(un, status)
    end if
  end subroutine

  subroutine write_fits(self, fn, errno)
    class(image_frame_t) :: self
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno

    write (0, *) 'image_frame_t % write_fits'

    call self % frame_t % write_fits(fn, errno)
  end subroutine

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

end module

!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!

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

  pure logical function endswith(buf, suff)
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
