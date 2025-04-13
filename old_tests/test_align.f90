program test_align

  use transforms
  use globals
  use findstar
  implicit none

  type(source), dimension(:), allocatable :: stars1, stars2

  call random_seed()

  generate_stars: block
    integer :: n
    real(fp), allocatable :: x(:)

    n = number_of_stars()
    allocate(stars1(n), stars2(n), x(n))

    call random_number(x)
    stars1(:) % x = 1 + x(:) * 1023

    call random_number(x)
    stars1(:) % y = 1 + x(:) * 767

    call random_number(x)
    stars1(:) % flux = exp(x(:) * log(100.0))

  end block generate_stars

contains

  integer function number_of_stars() result(n)
    real(fp) :: x
    call random_number(x)
    n = nint(30 * x) + 30
  end function

  subroutine comp_ydv(t, xy0, xy, k0, U, U_dv)
    
    class(transform), intent(in) :: t
    type(source), dimension(:), intent(in) :: xy0, xy
    type(source), dimension(:), allocatable :: xy1

    real(fp), intent(in) :: k0
    real(fp), intent(out) :: U, U_dv(:)
    real(fp), dimension(size(U_dv)) :: dx1_dv, dy1_dv

    real(fp) :: aa, bb, U_dx1, U_dy1
    integer :: i0, i1

    U = 0
    U_dv(:) = 0

    allocate(xy1(size(xy)))

    call t % apply(xy(:) % x, xy(:) % y, xy1(:) % x, xy1(:) % y)
    xy1(:) % flux = xy(:) % flux

    do i0 = 1, size(xy0)
      do i1 = 1, size(xy1)

        aa = sqrt((xy1(i1) % x - xy0(i0) % x)**2 &
        &       + (xy1(i1) % y - xy0(i0) % y)**2 + k0**2 )

        bb = sqrt(xy0(i0) % flux * xy1(i1) % flux) * k0

        U = U + bb / aa
        U_dx1 = - (xy1(i1) % x - xy0(i0) % x) * bb / aa**3
        U_dy1 = - (xy1(i1) % y - xy0(i0) % y) * bb / aa**3

        ! tutaj musi byc xy a nie xy1!!!
        call t % pder(xy(i1) % x, xy(i1) % y, dx1_dv, dy1_dv)
        U_dv(:) = U_dv(:) + U_dx1 * dx1_dv(:) + U_dy1 * dy1_dv(:)

      end do
    end do

    deallocate(xy1)

  end subroutine

end program test_align
