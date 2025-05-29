module legacy_frame_value_m

    use globals, only: img_k => fp
    use image_frame_m
    use value_m
    implicit none (type, external)
    private

    type, extends(value_t) :: legacy_frame_value_t
        type(image_frame_t) :: frame
        real(kind=img_k), allocatable :: data(:,:)
    contains
        procedure :: to_str => image_to_str
    end type

    public :: legacy_frame_value_t, img_k

contains

    pure function image_to_str(value) result(str)
        !! string repesentation of an image buffer
        class(legacy_frame_value_t), intent(in) :: value
        character(len=64) :: buf
        character(len=:), allocatable :: str

        if (.not. allocated(value%data)) then
            str = "<empty legacy image frame>"
            return
        end if

        associate (data => value%data)
            write(buf, "(i0,a,i0,a,f0.3,a)") size(data,1), "x", size(data, 2), &
                " image (exp=", value%frame%exptime, " sec)"
        end associate

        str = trim(adjustl(buf))
    end function

end module
