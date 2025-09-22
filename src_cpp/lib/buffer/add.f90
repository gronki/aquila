module addmod

    use iso_c_binding
    implicit none (type, external)

contains

    subroutine add_double_vec_vec(x, y, z, n) bind(C)
        real(c_double), intent(in) :: x(n), y(n)
        real(c_double), intent(inout) :: z(n)
        integer(c_size_t), intent(in), value :: n

        integer(c_size_t) :: i

        ! print *, "hello Fortran"

        !$omp simd
        do i = 1, n
            z(i) = x(i) + y(i)
        end do
    end subroutine

    subroutine add_double_vec_const(x, y, z, n) bind(C)
        real(c_double), intent(in) :: x(n)
        real(c_double), intent(in), value :: y
        real(c_double), intent(inout) :: z(n)
        integer(c_size_t), intent(in), value :: n

        integer(c_size_t) :: i

        ! print *, "hello Fortran"

        !$omp simd
        do i = 1, n
            z(i) = x(i) + y
        end do
    end subroutine

    subroutine add_float_vec_vec(x, y, z, n) bind(C)
        real(c_float), intent(in) :: x(n), y(n)
        real(c_float), intent(inout) :: z(n)
        integer(c_size_t), intent(in), value :: n

        integer(c_size_t) :: i

        ! print *, "hello Fortran"

        !$omp simd
        do i = 1, n
            z(i) = x(i) + y(i)
        end do
    end subroutine

    subroutine add_float_vec_const(x, y, z, n) bind(C)
        real(c_float), intent(in) :: x(n)
        real(c_float), intent(in), value :: y
        real(c_float), intent(inout) :: z(n)
        integer(c_size_t), intent(in), value :: n

        integer(c_size_t) :: i

        ! print *, "hello Fortran"

        !$omp simd
        do i = 1, n
            z(i) = x(i) + y
        end do
    end subroutine


end module