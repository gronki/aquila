module kernels

  use globals
  implicit none

contains

  !----------------------------------------------------------------------------!

  subroutine mexhakrn(sg,k)
    real(sp), intent(out) :: k(:,:)
    real(sp), intent(in) :: sg
    integer :: i, j, i1, j1
    real(sp) :: ci, cj
    real(sp) :: s(30)
    real(dp) :: tot

    do i = 1, size(s)
      s(i) = (i - 0.5) / size(s) - 0.5
    end do

    ci = (size(k,1) + 1) / 2.0
    cj = (size(k,2) + 1) / 2.0

    do j = 1, size(k,2)
      do i = 1, size(k,1)
        tot = 0
        do j1 = 1, size(s)
          do i1 = 1, size(s)
            tot = tot + f(real(i) - ci + s(i1), real(j) - cj + s(j1))
          end do
        end do
        k(i,j) = tot / size(s)**2
      end do
    end do

  contains
    elemental real function f(x,y) result(z)
      real, intent(in) :: x, y
      real(dp), parameter :: pi = 4 * atan(1d0)
      real :: k
      k = (x**2 + y**2) / (2 * sg**2)
      z =  (1 - k)  / (pi * sg**4) * exp(-k)
    end function
  end subroutine

  !----------------------------------------------------------------------------!

  function mexhakrn_alloc(fwhm) result(k)

    real(sp), intent(in) :: fwhm
    real(sp), allocatable :: k(:,:)
    integer :: n

    n = nint(7 * fwhm / 2.35)
    if (mod(n,2) == 0) n = n + 1
    allocate(k(n,n))

    call mexhakrn(fwhm / 2.35, k)

  end function

  !----------------------------------------------------------------------------!

  subroutine gausskrn(sg,k)
    real(sp), intent(out) :: k(:,:)
    real(sp), intent(in) :: sg
    integer :: i, j, i1, j1
    real(sp) :: ci, cj
    real(sp) :: s(20)
    real(dp) :: tot

    do i = 1, size(s)
      s(i) = (i - 0.5) / size(s) - 0.5
    end do

    ci = (size(k,1) + 1) / 2.0
    cj = (size(k,2) + 1) / 2.0

    do j = 1, size(k,2)
      do i = 1, size(k,1)
        tot = 0
        do j1 = 1, size(s)
          do i1 = 1, size(s)
            tot = tot + f(real(i) - ci + s(i1), real(j) - cj + s(j1))
          end do
        end do
        k(i,j) = tot / size(s)**2
      end do
    end do

    k = k / sum(k)

  contains
    elemental real function f(x,y) result(z)
      real, intent(in) :: x, y
      z = exp(-(x**2 + y**2) / (2 * sg**2))
    end function
  end subroutine

  !----------------------------------------------------------------------------!

  function gausskrn_alloc(fwhm) result(k)

    real(sp), intent(in) :: fwhm
    real(sp), allocatable :: k(:,:)
    integer :: n

    n = nint(6 * fwhm / 2.35)
    if (mod(n,2) == 0) n = n + 1
    allocate(k(n,n))

    call gausskrn(fwhm / 2.35, k)

  end function

  !----------------------------------------------------------------------------!

end module kernels
