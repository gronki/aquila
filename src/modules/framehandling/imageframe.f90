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