module convolutions

use globals
use fastconv
use aquila_c_binding

implicit none

contains

 !----------------------------------------------------------------------------!

subroutine convol(x,k,y)
   real(buf_k), dimension(:,:), intent(in), contiguous :: x, k
   real(buf_k), dimension(:,:), intent(out), contiguous :: y
   integer :: i, j, ri, rj

   if (mod(size(k,1), 2) == 0 .or. mod(size(k,2), 2) == 0)     &
      error stop "kernel must have uneven dimensions"

   call conv2d_pad(x, padded_2d_kernel(k, 8_size_k), size(k, 1, size_k), .true., y)

end subroutine

 !----------------------------------------------------------------------------!

pure subroutine build_image_expansion(x, lo_bound, hi_bound, method, y, errno)
   real(buf_k), dimension(:,:), intent(in), contiguous :: x
   real(buf_k), dimension(:,:), intent(inout), contiguous :: y
   character(len=*), intent(in) :: method
   integer(size_k), intent(in) :: lo_bound(2), hi_bound(2)
   integer(c_int), intent(out), optional :: errno
   integer(size_k) ::  in_shape(2), &
      out_shape(2), expected_out_shape(2), i ,j, yi, yj
   real(buf_k) :: fillval
   integer :: ioerr

   if (present(errno)) errno = 0

   in_shape = shape(x)
   expected_out_shape = 1 + hi_bound - lo_bound
   out_shape = shape(y)

   call pure_forbid(any(out_shape /= expected_out_shape), errno, 1, "wrong dimension")
   if (failed(errno)) return

   select case (method)
   case ('e')
      do concurrent (i = lo_bound(1):hi_bound(1), j = lo_bound(2):hi_bound(2))
         yi = i - lo_bound(1) + 1
         yj = j - lo_bound(2) + 1
         y(yi,yj) = x(ixlim(i, in_shape(1)), ixlim(j, in_shape(2)))
      end do
   case ('r')
      do concurrent (i = lo_bound(1):hi_bound(1), j = lo_bound(2):hi_bound(2))
         yi = i - lo_bound(1) + 1
         yj = j - lo_bound(2) + 1
         y(yi,yj) = x(ixrefl(i, in_shape(1)), ixrefl(j, in_shape(2)))
      end do
   case default
      read(method, *, iostat=ioerr) fillval
      call pure_forbid(ioerr /= 0, errno, 2,  "incorrect method of convolution image expansion")
      if (failed(errno)) return

      do concurrent (i = lo_bound(1):hi_bound(1), j = lo_bound(2):hi_bound(2))
         yi = i - lo_bound(1) + 1
         yj = j - lo_bound(2) + 1
         if (i < 1 .or. i > in_shape(1) .or. j < 1 .or. j > in_shape(2)) then
            y(yi,yj) = fillval
         else
            y(yi,yj) = x(i,j)
         end if
      end do
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

pure subroutine expand_image(x, shape_k, method, y, errno)
   real(buf_k), dimension(:,:), intent(in), contiguous :: x
   real(buf_k), dimension(:,:), intent(inout), contiguous :: y
   character(*), intent(in) :: method
   integer(c_int), intent(out), optional :: errno
   integer(size_k), intent(in) :: shape_k(2)
   integer(size_k) ::  in_shape(2), offset(2)

   in_shape = shape(x)
   offset = (shape_k - 1) / 2
   call build_image_expansion(x, &
      1 - offset, in_shape + offset, &
      method, y, errno)

end subroutine

subroutine conv2d_bind_c(x, k, method, y, parallel, errno) bind(C, name="conv2d_smallkernel")
   type(buffer_descriptor_t), intent(in), value :: x, k
   type(buffer_descriptor_t), intent(in), value :: y
   character(kind=c_char, len=1), intent(in) :: method(*)
   logical(kind=c_bool), intent(in), value :: parallel
   integer(c_int) :: errno

   real(buf_k), contiguous, pointer :: x_buf(:,:), k_buf(:,:), y_buf(:,:)
   character(kind=c_char, len=32) :: method_f

   x_buf => from_descriptor(x)
   k_buf => from_descriptor(k)
   y_buf => from_descriptor(y)

   call c_f_string(method, method_f)
   call conv2d_fix(x_buf, k_buf, method_f, y_buf, logical(parallel), errno)

end subroutine

subroutine conv2d_fix(x,k,method,y,parallel, errno)
   real(buf_k), dimension(:,:), intent(in), contiguous :: x, k
   real(buf_k), dimension(:,:), intent(inout), contiguous :: y
   integer(c_int), intent(out), optional :: errno
   character(*), intent(in) :: method
   logical, intent(in), optional :: parallel

   real(buf_k), allocatable :: padded_kernel(:,:), temp_input(:,:)
   integer(size_k) :: offset(2), input_shape(2)

   if (forbidden(any(mod(shape(k), 2) == 0), errno, &
      3, "conv2d: kernel must have uneven dimensions")) return

   input_shape = shape(x, size_k)

   if (forbidden(any(input_shape /= shape(y)), errno, &
      4, "conv2d: input and output shapes must match")) return

   offset = (shape(k, size_k) - 1) / 2
   padded_kernel = padded_2d_kernel(k, 8_size_k)

   call conv2d_pad(x, padded_kernel, size(k, 1, size_k), .false., &
      y(1 + offset(1) : input_shape(1) - offset(1), &
   & 1 + offset(2) : input_shape(2) - offset(2)), parallel)


   if (offset(2) > 0) then
      allocate(temp_input(input_shape(1) + 2 * offset(1), 3 * offset(2)))

      ! left strip
      call build_image_expansion(x, 1 - offset, &
         [input_shape(1) + offset(1), 2 * offset(2)], method, temp_input, errno)
      if (failed(errno)) return
      call conv2d_pad(temp_input, padded_kernel, size(k, 1, size_k), .false., &
         y(:, :offset(2)))

      ! roght strip
      call build_image_expansion(x, &
         [1 - offset(1), input_shape(2) - 2 * offset(2) + 1], &
         input_shape + offset, &
         method, temp_input, errno)
      if (failed(errno)) return
      call conv2d_pad(temp_input, padded_kernel, size(k, 1, size_k), .false., &
         y(:, input_shape(2) - offset(2) + 1:))
      deallocate(temp_input)
   end if

   if (offset(1) > 0) then
      allocate(temp_input(3 * offset(1), input_shape(2)))

      ! top padding
      call build_image_expansion(x, &
         [1 - offset(1), 1_size_k], &
         [2 * offset(1), input_shape(2)], &
         method, temp_input, errno)
      if (failed(errno)) return
      call conv2d_nopad(temp_input, k, .false., &
         y(:offset(1), 1 + offset(2) : input_shape(2) - offset(2)))

      ! bottom padding
      call build_image_expansion(x, &
         [input_shape(1) - 2 * offset(1) + 1, 1_size_k], &
         [input_shape(1) + offset(1), input_shape(2)], &
         method, temp_input, errno)
      if (failed(errno)) return
      call conv2d_nopad(temp_input, k, .false., &
         y(input_shape(1) - offset(1) + 1:, 1 + offset(2) : input_shape(2) - offset(2)))

      deallocate (temp_input)
   end if

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

subroutine deconvol_lr(im1, psf, strength, maxiter, im2, parallel)
   use ieee_arithmetic, only: ieee_is_normal

   real(buf_k), dimension(:,:), intent(in), contiguous :: im1, psf
   real(buf_k), dimension(:,:), intent(out), contiguous :: im2
   real(buf_k), intent(in) :: strength
   integer, intent(in) :: maxiter
   logical, intent(in), optional :: parallel
   real(buf_k), dimension(:,:), allocatable :: buf1, buf2, psf_inv
   real(buf_k) :: err1, err2, err01, err02
   integer(size_k) :: i

   if (size(im1,1) /= size(im2,1) .or. size(im1,2) /= size(im2,2)) &
      error stop "shape(im1) /= shape(im2)"

   psf_inv = psf(size(psf,1):1:-1, size(psf,2):1:-1)

   allocate(buf1, source=im1)
   allocate(buf2, source=im1)

   im2(:,:) = im1

#   ifndef NDEBUG
   write (0, '("+", 78("-"), "+")')
   write (0, '(a)') 'error 1: measures difference between original and deconvolved'
   write (0, '(a)') 'error 2: measures how much the image is altered in the iteration'
   write (0, '(a5, 4a12)') 'i', 'err1', 'd_err1', 'err2', 'd_err2'
#   endif

   deconv_loop: do i = 1, maxiter
      ! actual deconvolution
      call conv2d_fix(im2, psf, 'e', buf1, parallel)
      where (buf1 /= 0) buf1 = im1 / buf1
      call conv2d_fix(buf1, psf_inv, 'e', buf2, parallel)
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
