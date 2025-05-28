module buf3d_f32_value_m

    use iso_fortran_env, only: real32
    use value_m
    implicit none (type, external)
    private

    type buf3d_f32_t
        real(real32), allocatable :: data(:,:,:)
    end type

    type, extends(value_t) :: buf3d_f32_value_t
        class(buf3d_f32_t) :: buf
    contains
        procedure :: to_str => buf3d_f32_to_str
    end type

    public :: buf3d_f32_t, buf3d_f32_value_t

contains

    pure function buf3d_f32_to_str(value) result(str)
        !! string repesentation of an buf3d_f32 buffer
        class(buf3d_f32_value_t), intent(in) :: value
        character(len=64) :: buf
        character(len=:), allocatable :: str

        if (.not. allocated(value%buf%data)) then
            str = "empty buf3d_f32"
            return
        end if

        associate (data => value%buf%data)
            write(buf, "(i0,a,i0,a,i0,a)") size(data, 2), "x", size(data, 1), &
                " buf3d_f32 (", size(data, 3), " channels)"
        end associate

        str = trim(adjustl(buf))
    end function

end module
