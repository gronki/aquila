module convolutions

  use globals
  use fastconv
  implicit none

contains

  !----------------------------------------------------------------------------!

  pure subroutine convol(x,k,y)
    real(buf_k), dimension(:,:), intent(in), contiguous :: x, k
    real(buf_k), dimension(:,:), intent(out), contiguous :: y
    integer :: i, j, ri, rj

    if (mod(size(k,1), 2) == 0 .or. mod(size(k,2), 2) == 0)     &
        error stop "kernel must have uneven dimensions"

    call conv2d_pad(x, padded_2d_kernel(k, 4_size_k), size(k, 1, size_k), .true., y)

  end subroutine

  !----------------------------------------------------------------------------!

  pure subroutine convol_fix(x,k,y,method)
    real(buf_k), dimension(:,:), intent(in), contiguous :: x, k
    real(buf_k), dimension(:,:), intent(out), contiguous :: y
    real(buf_k), dimension(:,:), allocatable :: tmpx
    character(*), intent(in) :: method
    integer :: ri, rj, i, j

    if (mod(size(k,1), 2) == 0 .or. mod(size(k,2), 2) == 0)     &
        error stop "kernel must have uneven dimensions"

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

    call conv2d_pad(tmpx, padded_2d_kernel(k, 4_size_k), size(k, 1, size_k), .false., y)

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
      if (i < 1) then
        j = 1
      else if (i > n) then
        j = n
      else
        j = i
      end if
    end function
  end subroutine

  !----------------------------------------------------------------------------!

end module convolutions

!------------------------------------------------------------------------------!

module deconvolutions

  use globals, only: buf_k
  implicit none

contains

  subroutine deconvol_lr(im1, psf, strength, maxiter, im2)
    use convolutions, only: convol, convol_fix
    use ieee_arithmetic, only: ieee_is_normal

    real(buf_k), dimension(:,:), intent(in), contiguous :: im1, psf
    real(buf_k), dimension(:,:), intent(out), contiguous :: im2
    real(buf_k), intent(in) :: strength
    integer, intent(in) :: maxiter
    real(buf_k), dimension(:,:), allocatable :: buf1, buf2, psf_inv
    real(buf_k) :: err1, err2, err01, err02
    integer :: i

    if (size(im1,1) /= size(im2,1) .or. size(im1,2) /= size(im2,2)) &
      error stop "shape(im1) /= shape(im2)"

    psf_inv = psf(size(psf,1):1:-1, size(psf,2):1:-1)
    allocate(buf1(size(im1,1), size(im1,2)), buf2(size(im1,1), size(im1,2)))

    im2(:,:) = im1

#   if _DEBUG
      write (0, '("+", 78("-"), "+")')
      write (0, '(a)') 'error 1: measures difference between original and deconvolved'
      write (0, '(a)') 'error 2: measures how much the image is altered in the iteration'
      write (0, '(a5, 4a12)') 'i', 'err1', 'd_err1', 'err2', 'd_err2'
#   endif

    deconv_loop: do i = 1, maxiter
      ! actual deconvolution
      call convol_fix(im2, psf, buf1, 'e')
      where (buf1 /= 0) buf1 = im1 / buf1
      call convol_fix(buf1, psf_inv, buf2, 'e')
      im2(:,:) = im2 * buf2 * strength + im1 * (1 - strength)

#     if _DEBUG
        ! the rest is just error estimation
        err1 = sqrt(sum((buf1 - 1)**2) / size(im1))
        err2 = sqrt(sum((buf2 - 1)**2) / size(im1))

        write (0, '(i5, 4f12.5)') i, &
        err1, merge(err1 / err01 - 1, 0.0_buf_k, i > 1), &
        err2, merge(err2 / err02 - 1, 0.0_buf_k, i > 1)

        err01 = err1
        err02 = err2
#     endif
    end do deconv_loop

#   if _DEBUG
    write (0, '("+", 78("-"), "+")')
#   endif

    deallocate(buf1, buf2, psf_inv)
  end subroutine

end module deconvolutions
