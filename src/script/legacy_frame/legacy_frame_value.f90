module legacy_frame_value_m

    use globals, only: img_k => fp
    use image_frame_m
    use value_m
    use error_m
    implicit none (type, external)
    private

    type, extends(value_t) :: legacy_frame_value_t
        type(image_frame_t) :: frame
    contains
        procedure :: to_str => image_to_str
        procedure :: write => image_write
    end type

    public :: legacy_frame_value_t, img_k, as_frame

contains

    pure function image_to_str(value) result(str)
        !! string repesentation of an image buffer
        class(legacy_frame_value_t), intent(in) :: value
        character(len=64) :: buf
        character(len=:), allocatable :: str

        if (.not. allocated(value%frame%data)) then
            str = "<empty legacy image frame>"
            return
        end if

        associate (data => value%frame%data)
            write(buf, "(i0,a,i0,a,f0.3,a)") size(data,2), "x", size(data, 1), &
                " image (exp=", value%frame%exptime, " sec)"
        end associate

        str = trim(adjustl(buf))
    end function

    subroutine image_write(val, fn, err)
        class(legacy_frame_value_t), intent(in) :: val
        character(len=*), intent(in) :: fn
        type(err_t), intent(out) :: err
        integer :: errno
        
        errno = 0
        call val % frame % write_fits(fn, errno)
        if (errno /= 0) &
            call seterr(err, "writing FITS file failed")
     end subroutine

     subroutine as_frame(val, frame, err)
        class(value_t), target :: val
        type(legacy_frame_value_t), pointer :: frame
        type(err_t), intent(out), optional :: err

        select type(val)
        type is (legacy_frame_value_t)
           if (.not. allocated(val % frame % data)) then
              call seterr(err, "frame data not allocated")
              return
           end if
           frame => val
        class default
           call seterr( err, "expected image frame as `frame` parameter" )
        end select
     end subroutine

end module
