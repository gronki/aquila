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