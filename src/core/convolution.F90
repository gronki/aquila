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

   call conv2d_pad(x, padded_2d_kernel(k, 8_size_k), size(k, 1, size_k), .true., y)

end subroutine

 !----------------------------------------------------------------------------!

pure subroutine expand_image(x, shape_k, method, y)
   real(buf_k), dimension(:,:), intent(in), contiguous :: x
   real(buf_k), dimension(:,:), intent(inout), contiguous :: y
   character(*), intent(in) :: method
   integer(size_k), intent(in) :: shape_k(2)
   integer(size_k) ::  in_shape(2), offset(2), &
      out_shape(2), expected_out_shape(2), i ,j

   in_shape = shape(x)
   expected_out_shape = in_shape + shape_k - 1
   offset = (shape_k - 1) / 2
   out_shape = shape(y)
   if (any(out_shape /= expected_out_shape)) &
      error stop "wrong dimension"

   select case (method)
   case ("clone","expand","E",'e')
      do concurrent (i = 1:out_shape(1), j = 1:out_shape(2))
         y(i,j) = x(ixlim(i - offset(1), in_shape(1)), ixlim(j - offset(2), in_shape(2)))
      end do
   case ("reflect","reflection","R",'r')
      do concurrent (i = 1:out_shape(1), j = 1:out_shape(2))
         y(i,j) = x(ixrefl(i - offset(1), in_shape(1)), ixrefl(j - offset(2), in_shape(2)))
      end do
   case default
      error stop "method must be: clone or reflect"
   end select

contains

   elemental function ixrefl(i,n) result(iout)
      integer(size_k), intent(in) :: i,n
      integer(size_k) :: iout
      if (i < 1) then
         iout = 1 + abs(i - 1)
      else if (i > n) then
         iout = n - abs(n - i)
      else
         iout = i
      end if
   end function

   elemental function ixlim(i,n) result(iout)
      integer(size_k), intent(in) :: i,n
      integer(size_k) :: iout
      if (i < 1) then
         iout = 1
      else if (i > n) then
         iout = n
      else
         iout = i
      end if
   end function
end subroutine

pure subroutine convol_fix(x,k,y,method)
   real(buf_k), dimension(:,:), intent(in), contiguous :: x, k
   real(buf_k), dimension(:,:), intent(inout), contiguous :: y
   character(*), intent(in) :: method

   real(buf_k), dimension(:,:), allocatable :: tmpx

   if (mod(size(k,1), 2) == 0 .or. mod(size(k,2), 2) == 0)     &
      error stop "kernel must have uneven dimensions"

   allocate(tmpx(size(x, 1) + size(k, 1) - 1, size(x, 2) + size(k, 2) - 1))
   call expand_image(x, shape(k, kind=size_k), method, tmpx)
   call conv2d_pad(tmpx, padded_2d_kernel(k, 8_size_k), size(k, 1, size_k), .false., y)

end subroutine

 !----------------------------------------------------------------------------!

end module convolutions

!------------------------------------------------------------------------------!

module deconvolutions

use globals, only: buf_k
use fastconv
use convolutions

implicit none

contains

subroutine deconvol_lr(im1, psf, strength, maxiter, im2)
   use ieee_arithmetic, only: ieee_is_normal

   real(buf_k), dimension(:,:), intent(in), contiguous :: im1, psf
   real(buf_k), dimension(:,:), intent(out), contiguous :: im2
   real(buf_k), intent(in) :: strength
   integer, intent(in) :: maxiter
   real(buf_k), dimension(:,:), allocatable :: buf1, buf2, psf_inv, psf_pad, psf_inv_pad, im2_exp, buf1_exp
   real(buf_k) :: err1, err2, err01, err02
   integer(size_k) :: i, size_k_1

   if (size(im1,1) /= size(im2,1) .or. size(im1,2) /= size(im2,2)) &
      error stop "shape(im1) /= shape(im2)"

   size_k_1 = size(psf, 1)
   psf_pad = padded_2d_kernel(psf, 8_size_k)
   psf_inv = psf(size(psf,1):1:-1, size(psf,2):1:-1)
   psf_inv_pad = padded_2d_kernel(psf_inv, 8_size_k)

   allocate(buf1, source=im1)
   allocate(buf2, source=im1)

   allocate(im2_exp(size(im1,1) + size(psf,1) - 1, size(im1,2) + size(psf,2) - 1))
   allocate(buf1_exp(size(im1,1) + size(psf,1) - 1, size(im1,2) + size(psf,2) - 1))

   im2(:,:) = im1

#   ifndef NDEBUG
   write (0, '("+", 78("-"), "+")')
   write (0, '(a)') 'error 1: measures difference between original and deconvolved'
   write (0, '(a)') 'error 2: measures how much the image is altered in the iteration'
   write (0, '(a5, 4a12)') 'i', 'err1', 'd_err1', 'err2', 'd_err2'
#   endif

   deconv_loop: do i = 1, maxiter
      ! actual deconvolution
      call expand_image(im2, shape(psf, kind=size_k), 'e', im2_exp)
      call conv2d_pad(im2_exp, psf_pad, size_k_1, .false., buf1)
      where (buf1 /= 0) buf1 = im1 / buf1
      call expand_image(buf1, shape(psf, kind=size_k), 'e', buf1_exp)
      call conv2d_pad(buf1_exp, psf_inv_pad, size_k_1, .false., buf2)
      im2(:,:) = im2 * buf2 * strength + im1 * (1 - strength)

#     ifndef NDEBUG
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

#   ifndef NDEBUG
   write (0, '("+", 78("-"), "+")')
#   endif

end subroutine

end module deconvolutions
