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
  end type image_frame_t

  !----------------------------------------------------------------------------!

  interface image_frame_t
    module procedure :: image_frame_t_ctor
    module procedure :: image_frame_t_ctor_buf
    module procedure :: image_frame_t_ctor_file
  end interface image_frame_t

  !----------------------------------------------------------------------------!

  public :: strip_buffer

contains

  subroutine read_fits(self, fn, errno)
    class(image_frame_t), intent(inout) :: self
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno
    integer :: un, status, bsize

    status = 0

    call self % frame_t % read_fits(fn, status)
    if (present(errno)) errno = status

    if (status /= 0) then
      if (.not. present(errno)) &
        error stop "reading FITS file failed: " // fn
      return
    end if

    self % fn = fn
    call self % hdr % load(fn, errno)

    if ('EXPTIME' .in. self % hdr) &
      self % exptime = self % hdr % get_real('EXPTIME')
    if ('CCD-TEMP' .in. self % hdr) &
      self % ccdtemp = self % hdr % get_real('CCD-TEMP')
    if ('FRAME' .in. self % hdr) &
      self % frametyp = self % hdr % get_str('FRAME')

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

  function image_frame_t_ctor() result(fr)
    type(image_frame_t):: fr
  end function

  !----------------------------------------------------------------------------!

  function image_frame_t_ctor_file(file) result(fr)
    type(image_frame_t):: fr
    character(len=*), intent(in) :: file

    call fr % read_fits(file)
  end function

  !----------------------------------------------------------------------------!

  function image_frame_t_ctor_buf(buf) result(fr)
    type(image_frame_t):: fr
    real(fp), target, intent(in) :: buf(:,:)

    fr % data = buf
  end function

  !----------------------------------------------------------------------------!

  pure function strip_buffer(fr) result(stripped)
    type(image_frame_t), intent(in) :: fr
    type(image_frame_t) :: stripped

    stripped % ccdtemp  = fr % ccdtemp
    stripped % exptime  = fr % exptime
    stripped % fn       = fr % fn
    stripped % frametyp = fr % frametyp
    stripped % hdr      = fr % hdr

  end function


end module