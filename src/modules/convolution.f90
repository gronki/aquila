module convolution

  use globals
  implicit none

contains

  !----------------------------------------------------------------------------!

  pure subroutine convol(x,k,y)
    real(sp), intent(in) :: x(:,:), k(:,:)
    real(sp), intent(out) :: y(:,:)
    real(dp) :: total
    integer :: i, j, ri, rj, i1, j1

    if (mod(size(k,1), 2) == 0 .or. mod(size(k,2), 2) == 0)     &
        error stop "kernel must have uneven dimensions"

    ri = (size(k,1) - 1) / 2
    rj = (size(k,2) - 1) / 2

    do j = 1 + rj, size(x,2) - rj
      do i = 1 + ri, size(x,1) - ri
        total = 0
        do j1 = 1, size(k,2)
          do i1 = 1, size(k,1)
            total = total + k(i1, j1) * x(i - ri + i1 - 1, j - rj + j1 - 1)
          end do
        end do
        y(i,j) = total
      end do
    end do

  end subroutine

  !----------------------------------------------------------------------------!
  
end module convolution
