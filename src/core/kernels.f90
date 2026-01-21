module kernels

  use globals
  implicit none

  real(buf_k), dimension(3,3), parameter :: krn_bl3_1 &
  & = reshape([0, 1, 0, 1, 4, 1, 0, 1, 0], [3, 3]) / 8.0_buf_k
  real(buf_k), dimension(3,3), parameter :: krn_bl3_2 &
  & = reshape([1, 1, 1, 1, 8, 1, 1, 1, 1], [3, 3]) / 16.0_buf_k
  real(buf_k), dimension(3,3), parameter :: krn_bl3_3 &
  & = reshape([1, 3, 1, 3, 8, 3, 1, 3, 1], [3, 3]) / 24.0_buf_k

  real(r64_k), parameter :: fwhm_over_sigma = sqrt(8 * log(2.0_r64_k))

  integer, parameter, private :: nkrnsub = 16

contains

  !----------------------------------------------------------------------------!

  subroutine print_kernel(kernel)
    real(buf_k), intent(in) :: kernel(:,:)
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
    real(r64_k), intent(in) :: y
    real(r64_k) :: x

    x = sqrt(2 * log(1 / y))
  end function

  !----------------------------------------------------------------------------!

  subroutine mexhakrn(sg,k)
    real(r64_k), intent(in) :: sg
    real(buf_k), intent(out) :: k(:,:)

    integer :: i, j, i1, j1
    real(r64_k) :: ci, cj
    real(r64_k) :: s(nkrnsub)
    real(r64_k) :: tot

    do concurrent (i = 1:nkrnsub)
      s(i) = (i - 0.5_buf_k) / nkrnsub - 0.5_buf_k
    end do

    ci = (size(k,1) + 1) / 2.0_r64_k
    cj = (size(k,2) + 1) / 2.0_r64_k

    do j = 1, size(k,2)
      do i = 1, size(k,1)
        tot = 0
        do j1 = 1, nkrnsub
          do i1 = 1, nkrnsub
            tot = tot + f(real(i,r64_k) - ci + s(i1), real(j,r64_k) - cj + s(j1))
          end do
        end do
        k(i,j) = real(tot / nkrnsub**2, buf_k) 
      end do
    end do

  contains
    elemental function f(x,y) result(z)
      real(r64_k), intent(in) :: x, y
      real(r64_k) :: z
      real(r64_k), parameter :: pi = 4 * atan(1.0_buf_k)
      real(r64_k) :: kk

      kk = (x**2 + y**2) / (2 * sg**2)
      z = (1 - kk) / (pi * sg**4) * exp(-kk)
    end function
  end subroutine

  !----------------------------------------------------------------------------!

  function mexhakrn_alloc(fwhm) result(k)
    real(r64_k), intent(in) :: fwhm
    real(buf_k), allocatable :: k(:,:)

    integer :: n

    n = nint(9.0 * fwhm / fwhm_over_sigma)
    if (mod(n,2) == 0) n = n + 1
    n = max(n, 3)

    allocate(k(n,n))
    call mexhakrn(fwhm / fwhm_over_sigma, k)
  end function

  !----------------------------------------------------------------------------!

  subroutine gausskrn(sg,k)
    real(r64_k), intent(in) :: sg
    real(buf_k), intent(out) :: k(:,:)

    integer :: i, j, i1, j1
    real(r64_k) :: ci, cj
    real(r64_k) :: s(nkrnsub)
    real(r64_k) :: tot

    do concurrent (i = 1:nkrnsub)
      s(i) = (i - 0.5_r64_k) / nkrnsub - 0.5_r64_k
    end do

    ci = (size(k,1) + 1) / 2.0_r64_k
    cj = (size(k,2) + 1) / 2.0_r64_k

    do j = 1, size(k,2)
      do i = 1, size(k,1)
        tot = 0
        do j1 = 1, nkrnsub
          do i1 = 1, nkrnsub
            tot = tot + f(real(i,r64_k) - ci + s(i1), real(j,r64_k) - cj + s(j1))
          end do
        end do
        k(i,j) = real(tot / nkrnsub**2, buf_k) 
      end do
    end do

    k = k / sum(k)

  contains
    elemental function f(x,y) result(z)
      real(r64_k), intent(in) :: x, y
      real(r64_k) :: z
      z = exp(-(x**2 + y**2) / (2 * sg**2))
    end function
  end subroutine

  !----------------------------------------------------------------------------!

  function gausskrn_alloc(fwhm) result(k)
    real(r64_k), intent(in) :: fwhm
    real(buf_k), allocatable :: k(:,:)

    integer :: n

    n = nint(9.0 * fwhm / fwhm_over_sigma)
    if (mod(n, 2) == 0) n = n + 1
    n = max(n, 3)

    allocate(k(n,n))
    call gausskrn(fwhm / fwhm_over_sigma, k)
  end function

  !----------------------------------------------------------------------------!

  function get_kernel_size_c(fwhm) result(n) bind(C, name="get_kernel_size")
    real(r64_k), intent(in), value :: fwhm
    integer(i64_k) :: n

    n = nint(9.0 * fwhm / fwhm_over_sigma)
    if (mod(n,2) == 0) n = n + 1
    n = max(n, 3)
  end function

  subroutine mexhakrn_c(fwhm, kd) bind(C, name="mexhakrn")
    use aquila_c_binding

    real(r64_k), intent(in), value :: fwhm
    type(buffer_descriptor_t), intent(in), value :: kd

    real(buf_k), pointer, contiguous :: k(:,:)

    k => from_descriptor(kd)
    call mexhakrn(fwhm / fwhm_over_sigma, k)
    
  end subroutine


  subroutine gausskrn_c(fwhm, kd) bind(C, name="gausskrn")
    use aquila_c_binding

    real(r64_k), intent(in), value :: fwhm
    type(buffer_descriptor_t), intent(in), value :: kd

    real(buf_k), pointer, contiguous :: k(:,:)

    k => from_descriptor(kd)
    call gausskrn(fwhm / fwhm_over_sigma, k)

  end subroutine


end module kernels
