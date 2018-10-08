module convolution

  use globals
  implicit none

contains

  !----------------------------------------------------------------------------!

  pure subroutine convol(x,k,y)
    real(fp), dimension(:,:), intent(in) :: x, k
    real(fp), dimension(:,:), intent(out) :: y
    real(fp) :: total
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
        y(i - ri, j - rj) = total
      end do
    end do

  end subroutine

  !----------------------------------------------------------------------------!

  pure subroutine convol_fix(x,k,y,method)
    real(fp), dimension(:,:), intent(in) :: x, k
    real(fp), dimension(:,:), intent(out) :: y
    real(fp), dimension(:,:), allocatable :: tmpx
    character(*), intent(in) :: method
    integer :: ri, rj, i, j

    call convol(x,k,y)

    ri = (size(k,1) - 1) / 2
    rj = (size(k,2) - 1) / 2

    allocate(tmpx(1 - ri : size(x,1) + ri, 1 - rj : size(x,2) + rj))

    select case (method)
    case ("clone","expand","E",'e')
      do j = lbound(tmpx, 2), ubound(tmpx, 2)
        do i = lbound(tmpx, 1), ubound(tmpx, 1)
          tmpx(i, j) = x(ixlim(i, size(x,1)), ixlim(j, size(x,2)))
        end do
      end do
    case ("reflect","reflection","R",'r')
      do j = lbound(tmpx, 2), ubound(tmpx, 2)
        do i = lbound(tmpx, 1), ubound(tmpx, 1)
          tmpx(i, j) = x(ixrefl(i, size(x,1)), ixrefl(j, size(x,2)))
        end do
      end do
    case default
      error stop "method must be: clone or reflect"
    end select

    call convol(tmpx, k, y)
    deallocate(tmpx)

  contains
    elemental integer function ixrefl(i,n) result(j)
      integer, intent(in) :: i,n
      if (i < 1) then
        j = 1 + abs(i - 1)
      else if (i > n) then
        j = n - abs(n - i)
      else
        j = i
      end if
    end function

    elemental integer function ixlim(i,n) result(j)
      integer, intent(in) :: i,n
      j = merge(1, merge(n, i, i > n), i < 1)
    end function
  end subroutine

  !----------------------------------------------------------------------------!

end module convolution
