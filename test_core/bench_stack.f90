program bench_stack
    use stacking
    implicit none (type, external)

    real(buf_k), allocatable :: buf(:,:,:), result(:,:)

    ! call random_seed()
    ! allocate(buf(4000,6000,128))
    ! allocate(result(size(buf,1), size(buf,2)))
    ! call random_number(buf)

    ! print *, 'bench 1'
    ! call bench_1("average")

contains

    subroutine bench_1(method)
        character(len=*) :: method

        ! call stack_buffer()

    end subroutine

end program
