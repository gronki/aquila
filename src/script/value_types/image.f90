module image_m

    use iso_fortran_env, only: real32
    use value_base_m
    implicit none (type, external)
    private

    type image_t
        real(real32), allocatable :: data(:,:,:)
    end type

    type, extends(value_t) :: image_value_t
        class(image_t), allocatable :: image
    contains
        procedure :: to_str => image_to_str
    end type

    public :: image_t, image_value_t

contains

    pure function image_to_str(value) result(str)
        !! string repesentation of an image buffer
        class(image_value_t), intent(in) :: value
        character(len=64) :: buf
        character(len=:), allocatable :: str

        if (.not. allocated(value%image)) error stop
        if (.not. allocated(value%image%data)) error stop

        associate (data => value%image%data)
            write(buf, "(i0,a,i0,a,i0,a)") size(data,1), "x", size(data, 2), &
                " image (", size(data,3), " channels)"
        end associate

        str = trim(adjustl(buf))
    end function

end module
