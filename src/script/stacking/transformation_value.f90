module transformation_value_m

    use value_m
    use error_m
    use new_align
    implicit none (type, external)
    private

    type, extends(value_t) :: transformation_value_t
        class(transform_t), allocatable :: transform
    contains
        procedure :: to_str
    end type

    public :: transformation_value_t

contains

    pure function to_str(value) result(str)
        !! string repesentation of an image buffer
        class(transformation_value_t), intent(in) :: value
        character(len=128) :: buf
        character(len=:), allocatable :: str

        if (.not. allocated(value%transform%vec)) then
            str = "T[empty]"
            return
        end if

        associate (v => value%transform%vec)
            write(buf, "(a, *(f8.3,:,', '))") "T[", v
        end associate

        str = trim(adjustl(buf)) // "]"
    end function

end module