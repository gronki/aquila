module kernels

  use globals
  implicit none

  real(fp), dimension(3,3), parameter :: krn_bl3_1 &
  & = reshape([0, 1, 0, 1, 4, 1, 0, 1, 0], [3, 3]) / 8.0_fp
  real(fp), dimension(3,3), parameter :: krn_bl3_2 &
  & = reshape([1, 1, 1, 1, 8, 1, 1, 1, 1], [3, 3]) / 16.0_fp
  real(fp), dimension(3,3), parameter :: krn_bl3_3 &
  & = reshape([1, 3, 1, 3, 8, 3, 1, 3, 1], [3, 3]) / 24.0_fp

  real(fp), parameter :: fwhm_over_sigma = sqrt(8 * log(2.0_fp))

  integer, parameter, private :: nkrnsub = 16

contains

  !----------------------------------------------------------------------------!

  subroutine print_kernel(kernel)
    real(fp), intent(in) :: kernel(:,:)
    integer :: i, sz(2)
    sz = shape(kernel)
    associate (kc => kernel((sz(1) + 1) / 2, (sz(2) + 1) / 2))
      print '(a,a)', ' +', repeat('--------+', sz(2))
      print_kernel_rows: do i = 1, sz(1)
        write (*, '(" |", *(f7.4," |"))') kernel(i,:) / kc
        print '(a,a)', ' +', repeat('--------+', sz(2))
      end do print_kernel_rows
    end associate
  end subroutine

  !----------------------------------------------------------------------------!

  elemental function howmanysigmas(y) result(x)
    real(fp), intent(in) :: y
    real(fp) :: x
    x = sqrt(2 * log(1 / y))
  end function

  !----------------------------------------------------------------------------!

  subroutine mexhakrn(sg,k)
    real(fp), intent(out) :: k(:,:)
    real(fp), intent(in) :: sg
    integer :: i, j, i1, j1
    real(fp) :: ci, cj
    real(fp) :: s(nkrnsub)
    real(fp) :: tot

    do concurrent (i = 1:nkrnsub)
      s(i) = (i - 0.5_fp) / nkrnsub - 0.5_fp
    end do

    ci = (size(k,1) + 1) / 2.0_fp
    cj = (size(k,2) + 1) / 2.0_fp

    do j = 1, size(k,2)
      do i = 1, size(k,1)
        tot = 0
        do j1 = 1, nkrnsub
          do i1 = 1, nkrnsub
            tot = tot + f(real(i,fp) - ci + s(i1), real(j,fp) - cj + s(j1))
          end do
        end do
        k(i,j) = tot / nkrnsub**2
      end do
    end do

  contains
    elemental function f(x,y) result(z)
      real(fp), intent(in) :: x, y
      real(fp) :: z
      real(fp), parameter :: pi = 4 * atan(1.0_fp)
      real(fp) :: k
      k = (x**2 + y**2) / (2 * sg**2)
      z = (1 - k) / (pi * sg**4) * exp(-k)
    end function
  end subroutine

  !----------------------------------------------------------------------------!

  function mexhakrn_alloc(fwhm) result(k)
    real(fp), intent(in) :: fwhm
    real(fp), allocatable :: k(:,:)
    integer :: n

    n = nint(9.0 * fwhm / fwhm_over_sigma)
    if (mod(n,2) == 0) n = n + 1
    n = max(n, 3)

    allocate(k(n,n))
    call mexhakrn(fwhm / fwhm_over_sigma, k)
  end function

  !----------------------------------------------------------------------------!

  subroutine gausskrn(sg,k)
    real(fp), intent(out) :: k(:,:)
    real(fp), intent(in) :: sg
    integer :: i, j, i1, j1
    real(fp) :: ci, cj
    real(fp) :: s(nkrnsub)
    real(fp) :: tot

    do concurrent (i = 1:nkrnsub)
      s(i) = (i - 0.5_fp) / nkrnsub - 0.5_fp
    end do

    ci = (size(k,1) + 1) / 2.0_fp
    cj = (size(k,2) + 1) / 2.0_fp

    do j = 1, size(k,2)
      do i = 1, size(k,1)
        tot = 0
        do j1 = 1, nkrnsub
          do i1 = 1, nkrnsub
            tot = tot + f(real(i,fp) - ci + s(i1), real(j,fp) - cj + s(j1))
          end do
        end do
        k(i,j) = tot / nkrnsub**2
      end do
    end do

    k = k / sum(k)

  contains
    elemental function f(x,y) result(z)
      real(fp), intent(in) :: x, y
      real(fp) :: z
      z = exp(-(x**2 + y**2) / (2 * sg**2))
    end function
  end subroutine

  !----------------------------------------------------------------------------!

  function gausskrn_alloc(fwhm) result(k)
    real(fp), intent(in) :: fwhm
    real(fp), allocatable :: k(:,:)
    integer :: n

    n = nint(9.0 * fwhm / fwhm_over_sigma)
    if (mod(n, 2) == 0) n = n + 1
    n = max(n, 3)

    allocate(k(n,n))
    call gausskrn(fwhm / fwhm_over_sigma, k)
  end function

  !----------------------------------------------------------------------------!

end module kernels
