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
    procedure :: read_fits, write_fits, check_shape
    procedure, private :: read_image_data
    procedure :: assign_data, assign_frame
    generic :: copy => assign_data, assign_frame
    procedure, private :: repr
    generic :: write(formatted) => repr
    procedure :: frame_destroy
    generic :: destroy => frame_destroy
  end type

  interface frame_t
    module procedure :: frame_t_ctor
    module procedure :: frame_t_copy_ctor
  end interface

contains

  !----------------------------------------------------------------------------!

  pure function frame_t_copy_ctor(ref) result(fr)
    type(frame_t):: fr
    class(frame_t), intent(in) :: ref

    call fr % copy(ref)
  end function

  !----------------------------------------------------------------------------!

  function frame_t_ctor(file, buf) result(fr)
    type(frame_t):: fr
    character(len=*), intent(in), optional :: file
    real(fp), target, intent(in), optional :: buf(:,:)

    if (present(buf)) fr % data => buf
    if (present(file)) call fr % read_fits(file)
  end function

  !----------------------------------------------------------------------------!

  pure subroutine check_shape(self, nx, ny)
    class(frame_t), intent(inout) :: self
    integer, intent(in) :: nx, ny

    if (associated(self % data)) then
      if (size(self%data, 1) == nx .and. size(self%data, 2) == ny) then
        return
      end if

      if (self%auto_allocated) then
        deallocate(self%data)
      else
        error stop 'check_shape: frame size does not match'
      end if
    end if

    self%auto_allocated = .true.
    allocate(self%data(nx, ny))
  end subroutine

  !----------------------------------------------------------------------------!

  elemental subroutine assign_frame(fr, ref)
    class(frame_t), intent(inout) :: fr
    type(frame_t), intent(in) :: ref

    if (.not. associated(ref % data)) error stop
    call fr % assign_data(ref % data)
  end subroutine

  !----------------------------------------------------------------------------!

  pure subroutine assign_data(fr, im)
    class(frame_t), intent(inout) :: fr
    real(fp), dimension(:,:), intent(in) :: im

    call fr % check_shape(size(im, 1), size(im, 2))
    fr % data(:,:) = im(:,:)
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

    if (.not. associated(self % data)) then
      write (u, '(a)', iostat=iostat, iomsg=iomsg) 'frame_t(unallocated)'
    else
      write (u, '("frame_t(", i0, ",", i0, ")")', &
        iostat=iostat, iomsg=iomsg) shape(self % data)
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  elemental impure subroutine frame_destroy(self)
    class(frame_t), intent(inout) :: self
    if (self % auto_allocated .and. associated(self % data)) then
      deallocate(self % data)
      self%auto_allocated = .false.
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

  integer, parameter :: len_key = 8, len_val = 70

  type fhentry
    character(len=len_key) :: key = ''
    character(len=len_val) :: value = ''
  end type

  type, public :: fhdict
    type(fhentry), allocatable, private :: list(:)
    integer, private :: n_entries = 0
  contains
    procedure :: add_str, add_real, add_int
    generic :: add => add_str, add_real, add_int

    procedure       :: get_str_ex, get_int_ex, get_real_ex
    generic :: get  => get_str_ex, get_int_ex, get_real_ex
    procedure       :: get_str   , get_int   , get_real   

    procedure :: has_key, erase, nrecords
    procedure, private :: add_raw, get_raw

    procedure :: read_from_file, read_from_unit
    generic :: load => read_from_file, read_from_unit

    procedure :: write_to_file, write_to_unit
    generic :: dump => write_to_file, write_to_unit

    ! procedure, private :: clone_from
    ! generic :: assignment(=) => clone_from

    procedure, private :: fhdict_repr
    generic :: write(formatted) => fhdict_repr
    procedure, pass(self), private :: has_key_op
    generic :: operator(.in.) => has_key_op
    ! final :: finalize
  end type

  character(len = 12), parameter :: excludes(*) = [character(12) :: 'END', 'COMMENT', &
    'SIMPLE', 'BITPIX', 'NAXIS', 'NAXIS1', 'NAXIS2', 'NAXIS3', 'EXTEND', 'BSCALE', 'BZERO']

  integer, parameter :: n_chunk = 8

contains

  !----------------------------------------------------------------------------!

  elemental function has_key(self, k) result(has)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    integer :: i
    logical :: has

    has = .false.

    do i = 1, self%n_entries
      if (self%list(i)%key == k) then
        has = .true.
        return
      end if
    end do

  end function

  !------------------------------------------------------------------------------!

  elemental logical function has_key_op(k, self) result(has)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    has = self % has_key(k)
  end function

  !----------------------------------------------------------------------------!

  pure subroutine add_raw(self, k, v)
    class(fhdict), intent(inout) :: self
    character(len = *), intent(in) :: k, v
    integer :: i_add

    if (k == '') error stop

    if (allocated(self % list)) then
      if (self%n_entries >= size(self%list)) then
        block
          type(fhentry), allocatable :: list_new(:)
          allocate(list_new(size(self%list) + n_chunk))
          list_new(1:self%n_entries) = self%list(1:self%n_entries)
          call move_alloc(from=list_new, to=self%list)
        end block
      end if
    else
      allocate(self%list(n_chunk))
      self%n_entries = 0
    end if

    self%n_entries = self%n_entries + 1
    self%list(self%n_entries)%key = k
    self%list(self%n_entries)%value = v
  end subroutine

  !----------------------------------------------------------------------------!

  pure subroutine add_int(self, k, v)
    class(fhdict), intent(inout) :: self
    character(len = *), intent(in) :: k
    integer, intent(in) :: v
    character(len=len_val) :: buf

    write (buf, *) v
    call self % add_raw(k, buf)
  end subroutine

  !------------------------------------------------------------------------------!

  pure subroutine add_real(self, k, v)
    class(fhdict), intent(inout) :: self
    character(len = *), intent(in) :: k
    real, intent(in) :: v
    character(len=len_val) :: buf

    write (buf, *) v
    call self % add_raw(k, buf)
  end subroutine

  !------------------------------------------------------------------------------!

  pure subroutine add_str(self, k, v)
    class(fhdict), intent(inout) :: self
    character(len = *), intent(in) :: k
    character(len = *), intent(in) :: v
    character(len=len_val) :: buf

    call self % add_raw(k, "'" // trim(v) // "'")
  end subroutine

  !----------------------------------------------------------------------------!

  elemental subroutine get_raw(self, k, buf, errno)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    character(len = *), intent(inout) :: buf
    integer, intent(out), optional :: errno
    integer :: i

    if (present(errno)) errno = 0

    do i = 1, self%n_entries
      if (self%list(i)%key == k) then
        buf = self%list(i)%value
        return
      end if
    end do

    ! not found
    if (present(errno)) then
      errno = -1
    else
      error stop 'key ' // trim(k) // ' not in the dict'
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  elemental subroutine get_str_ex(self, k, v, errno) 
    class(fhdict), intent(in) :: self
    character(len=*), intent(in) :: k
    character(len=*), intent(out) :: v
    integer, intent(out), optional :: errno
    character(len=len_val) :: buf
    integer :: err

    err = 0

    try: block
    
      call self % get_raw(k, buf, err)
      if (err /= 0) exit try

      read (buf, *, iostat=err) v
      if (err /= 0) exit try

    end block try

    if (present(errno)) then
      errno = err
    else if (err /= 0) then
      error stop
    end if

  end subroutine

  elemental function get_str(self, k, defv) result(v)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    character(len=len_val) :: v
    character(len=*), intent(in), optional :: defv
    integer :: err

    call self % get_str_ex(k, v, err)
    if (err == 0) return
    if (present(defv)) then
      v = defv
    else
      error stop 'value of ' // trim(k) // ' not retrieved but no default provided'
    end if
  end function

  !----------------------------------------------------------------------------!

  elemental subroutine get_int_ex(self, k, v, errno) 
    class(fhdict), intent(in) :: self
    character(len=*), intent(in) :: k
    integer, intent(out) :: v
    integer, intent(out), optional :: errno
    character(len=len_val) :: buf
    integer :: err

    err = 0

    try: block
    
      call self % get_raw(k, buf, err)
      if (err /= 0) exit try

      read (buf, *, iostat=err) v
      if (err /= 0) exit try

    end block try

    if (present(errno)) then
      errno = err
    else if (err /= 0) then
      error stop
    end if

  end subroutine

  elemental function get_int(self, k, defv) result(v)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    integer :: v
    integer, intent(in), optional :: defv
    integer :: err

    call self % get_int_ex(k, v, err)
    if (err == 0) return
    if (present(defv)) then
      v = defv
    else
      error stop 'value of ' // trim(k) // ' not retrieved but no default provided'
    end if
  end function

  !----------------------------------------------------------------------------!

  elemental subroutine get_real_ex(self, k, v, errno) 
    class(fhdict), intent(in) :: self
    character(len=*), intent(in) :: k
    real, intent(out) :: v
    integer, intent(out), optional :: errno
    character(len=len_val) :: buf
    integer :: err

    err = 0

    try: block

      call self % get_raw(k, buf, err)
      if (err /= 0) exit try

      read (buf, *, iostat=err) v
      if (err /= 0) exit try

    end block try

    if (present(errno)) then
      errno = err
    else if (err /= 0) then
      error stop
    end if

  end subroutine

  elemental function get_real(self, k, defv) result(v)
    class(fhdict), intent(in) :: self
    character(len = *), intent(in) :: k
    real :: v
    real, intent(in), optional :: defv
    integer :: err

    call self % get_real_ex(k, v, err)
    if (err == 0) return
    if (present(defv)) then
      v = defv
    else
      error stop 'value of ' // trim(k) // ' not retrieved but no default provided'
    end if
  end function

  !----------------------------------------------------------------------------!

  elemental function nrecords(self) result(nrec)
    class(fhdict), intent(in) :: self
    integer :: nrec

    nrec = self%n_entries
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

    if (allocated(self % list)) then
      write (u, '(a)', iostat = iostat, iomsg = iomsg) '[ '
      do i = 1, self%n_entries
        write (u, '(a, " => ", a, a)', iostat = iostat, iomsg = iomsg) &
        &   trim(self%list(i)%key), trim(self%list(i)%value), &
        &   merge(', ', ' ]', i < self%n_entries)
      end do
    else
      write (u, '(a)', iostat = iostat, iomsg = iomsg) '<empty fhdict>'
    end if
  end subroutine

  ! !----------------------------------------------------------------------------!

  ! elemental impure subroutine finalize(self)
  !   type(fhdict), intent(inout) :: self
  !   print '(a, 1x, dt)', 'kaboom', self
  !   call self % erase
  ! end subroutine

  !------------------------------------------------------------------------------!

  elemental subroutine erase(self)
    class(fhdict), intent(inout) :: self
    
    if (allocated(self%list)) deallocate(self%list)
    self%n_entries = 0
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine read_from_unit(self, un)
    class(fhdict), intent(inout) :: self
    integer, intent(in) :: un
    integer :: status
    character(len = 256) :: fn, comment
    character(len=len_key) :: key
    character(len=len_val) :: val
    integer :: nkeys, ikey

    status = 0

    next_kw: do
      call ftghps(un, nkeys, ikey, status)
      if (ikey > nkeys) exit next_kw
      call ftgkyn(un, ikey, key, val, comment, status)
      if (status /= 0) exit next_kw
      if (key == 'COMMENT' .or. key == 'END') cycle next_kw
      call self % add_raw(key, val)
    end do next_kw
  end subroutine

  !------------------------------------------------------------------------------!

  subroutine read_from_file(self, fn, errno)
    class(fhdict), intent(inout) :: self
    character(len = *), intent(in) :: fn
    integer, intent(out), optional :: errno
    integer :: un, bsize, status

    status = 0

    call ftgiou(un, status)
    call ftdkopn(un, fn, 0, bsize, status)

    if (status /= 0) then
      if (present(errno)) then
        errno = status; return
      else
        error stop "reading header from file"
      end if
    end if

    call self % erase
    call self % read_from_unit(un)

    call ftclos(un, status)
    call ftfiou(un, status)

    if (status /= 0) then
      if (present(errno)) then
        errno = status; return
      else
        error stop "reading header from file"
      end if
    end if

    if (present(errno)) errno = 0
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine write_to_unit(self, un, errno)
    class(fhdict), intent(inout) :: self
    integer, intent(out), optional :: errno
    integer, intent(in) :: un
    integer :: status, i
    character(len = 128) :: buf


    status = 0

    iter_keys: do i = 1, self%n_entries
      associate (cur => self%list(i))
        if (any(cur % key == excludes)) cycle
        write (buf, '(a8, "= ", a70)') cur % key, cur % value
        call ftprec(un, buf, status)
        if (status /= 0) then
          if (.not. present(errno)) error stop 'writing FITS keyword'
          errno = status; exit
        end if
      end associate
    end do iter_keys
  end subroutine

  !------------------------------------------------------------------------------!

  subroutine write_to_file(self, fn, errno)
    class(fhdict), intent(inout) :: self
    character(len = *), intent(in) :: fn
    integer, intent(out), optional :: errno
    integer :: un, bsize, status

    status = 0

    call ftgiou(un, status)
    call ftdkopn(un, fn, 1, bsize, status)

    if (status /= 0) then
      if (.not.present(errno)) error stop "writing FITS header to file"
      errno = status; return
    end if

    call fthdef(un, self % nrecords(), status)
    call self % write_to_unit(un, status)

    call ftclos(un, status)
    call ftfiou(un, status)

    if (status /= 0) then
      if (.not.present(errno)) error stop "writing FITS header to file"
      errno = status; return
    end if

    if (present(errno)) errno = 0
  end subroutine

  !------------------------------------------------------------------------------!

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
    ! procedure :: image_repr
    ! generic :: write(formatted) => image_repr
    ! procedure :: assign_image_frame
    ! generic :: assignment(=) => assign_image_frame
  end type image_frame_t

  !----------------------------------------------------------------------------!

  interface image_frame_t
  !   module procedure :: image_frame_t_copy_ctor
    module procedure :: image_frame_t_ctor
  end interface image_frame_t

  !----------------------------------------------------------------------------!

contains

  subroutine read_fits(self, fn, errno)
    class(image_frame_t), intent(inout) :: self
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno
    integer :: un, status, bsize

    status = 0

    call self % frame_t % read_fits(fn, status)
    if (present(errno)) errno = status

    if (status == 0) then
      self % fn = fn
      call self % hdr % load(fn, errno)

      if ('EXPTIME' .in. self % hdr) &
        self % exptime = self % hdr % get_real('EXPTIME')
      if ('CCD-TEMP' .in. self % hdr) &
        self % ccdtemp = self % hdr % get_real('CCD-TEMP')
      if ('FRAME' .in. self % hdr) &
        self % frametyp = self % hdr % get_str('FRAME')
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine write_fits(self, fn, errno)
    class(image_frame_t) :: self
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno

    call self % frame_t % write_fits(fn, errno)
    call self % hdr % dump(fn, errno)
  end subroutine

  !----------------------------------------------------------------------------!

  ! pure function image_frame_t_copy_ctor(ref) result(fr)
  !   type(image_frame_t):: fr
  !   class(image_frame_t), intent(in) :: ref

  !   call fr % assign_frame(ref)
  ! end function

  !----------------------------------------------------------------------------!

  function image_frame_t_ctor(file, buf) result(fr)
    type(image_frame_t):: fr
    character(len=*), intent(in), optional :: file
    real(fp), target, intent(in), optional :: buf(:,:)

    if (present(buf)) fr % data => buf
    if (present(file)) call fr % read_fits(file)
  end function

  !----------------------------------------------------------------------------!

!   elemental impure subroutine assign_image_frame(fr, ref)
!     class(image_frame_t), intent(inout) :: fr
!     type(image_frame_t), intent(in) :: ref

! #   if _DEBUG
!     print '("image assignment ", dt, " = ", dt)', fr, ref
! #   endif

!     fr % frame_t = ref % frame_t
!     fr % hdr = ref % hdr
!     fr % fn = ref % fn

!   end subroutine

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
