module fastconv

use iso_c_binding
use iso_fortran_env, only: error_unit

implicit none (type, external)

integer, parameter :: real_k = c_float
integer, parameter :: size_k = c_int64_t

contains

!> basic subroutine for 1D convolution, pure Fortran
pure subroutine conv1d_core(x, k, y)
   !> vector to be convolved
   real(real_k), intent(in), contiguous :: x(:)
   !> convolution kernel (should be reversed beforehand)
   real(real_k), intent(in), contiguous :: k(:)
   !> output vector, length size(x) + 1 - size(k)
   real(real_k), intent(out), contiguous :: y(:)

   integer(kind=size_k) :: input_size, kernel_size, output_size_raw

   input_size = size(x, kind=size_k)
   kernel_size = size(k, kind=size_k)
   output_size_raw = input_size - kernel_size + 1_size_k

#       ifndef NDEBUG
   if (size(y, kind=size_k) /= output_size_raw) &
      error stop 'incorrect output shape for 1D convolution'
#       endif

   if (modulo(kernel_size, 16) == 0) then
      call conv1d_k16(x, k, y)
   else if (modulo(kernel_size, 8) == 0) then
      call conv1d_k8(x, k, y)
   else if (modulo(kernel_size, 4) == 0) then
      call conv1d_k4(x, k, y)
   else if (kernel_size == 1) then
      y(:) = x * k(1)
   else
      call conv1d_general(x, k, y)
   end if

end subroutine

 !> compute the convolution for any kernel length
pure subroutine conv1d_general(x, k, y)
   !> vector to be convolved
   real(real_k), intent(in), contiguous :: x(:)
   !> convolution kernel (should be reversed beforehand)
   real(real_k), intent(in), contiguous :: k(:)
   !> output vector, length size(x) + 1 - size(k)
   real(real_k), intent(out), contiguous :: y(:)
   integer(kind=size_k) :: i, j, kernel_size, output_size
   real(real_k) :: total

   kernel_size = size(k, kind=size_k)
   output_size = size(x, kind=size_k) - kernel_size + 1_size_k

   do i = 1, output_size
      total = 0
      do j = 1, kernel_size
         total = total + k(j) * x(i + j - 1)
      end do
      y(i) = total
   end do

end subroutine

 !> specific implementation for multiplies of 8
pure subroutine conv1d_k16(x, k, y)
   !> vector to be convolved
   real(real_k), intent(in), contiguous :: x(:)
   !> convolution kernel (should be reversed beforehand)
   real(real_k), intent(in), contiguous :: k(:)
   !> output vector, length size(x) + 1 - size(k)
   real(real_k), intent(out), contiguous :: y(:)
   integer(kind=size_k) :: i, j, kernel_size_16, output_size
   real(real_k) :: total

#       ifndef NDEBUG
   if (modulo(size(k), 16) /= 0) error stop "size of kernel must be multiply of 16"
#       endif

   kernel_size_16 = size(k) / 16
   output_size = size(x) - 16 * kernel_size_16 + 1

   do i = 1, output_size
      total = 0
      do j = 1, kernel_size_16 * 16
         total = total + k(j) * x(i + j - 1)
      end do
      y(i) = total
   end do

end subroutine

 !> specific implementation for multiplies of 8
pure subroutine conv1d_k8(x, k, y)
   !> vector to be convolved
   real(real_k), intent(in), contiguous :: x(:)
   !> convolution kernel (should be reversed beforehand)
   real(real_k), intent(in), contiguous :: k(:)
   !> output vector, length size(x) + 1 - size(k)
   real(real_k), intent(out), contiguous :: y(:)
   integer(kind=size_k) :: i, j, kernel_size_8, output_size
   real(real_k) :: total

#       ifndef NDEBUG
   if (modulo(size(k), 8) /= 0) error stop "size of kernel must be multiply of 8"
#       endif

   kernel_size_8 = size(k) / 8
   output_size = size(x) - 8 * kernel_size_8 + 1

   do i = 1, output_size
      total = 0
      do j = 1, kernel_size_8 * 8
         total = total + k(j) * x(i + j - 1)
      end do
      y(i) = total
   end do

end subroutine

 !> specific implementation for multiplies of 4
pure subroutine conv1d_k4(x, k, y)
   !> vector to be convolved
   real(real_k), intent(in), contiguous :: x(:)
   !> convolution kernel (should be reversed beforehand)
   real(real_k), intent(in), contiguous :: k(:)
   !> output vector, length size(x) + 1 - size(k)
   real(real_k), intent(out), contiguous :: y(:)
   integer(kind=size_k) :: i, j, kernel_size_4, output_size
   real(real_k) :: total

#       ifndef NDEBUG
   if (modulo(size(k), 4) /= 0) error stop "size of kernel must be multiply of 4"
#       endif

   kernel_size_4 = size(k) / 4
   output_size = size(x) - 4 * kernel_size_4 + 1

   do i = 1, output_size
      total = 0
      do j = 1, kernel_size_4 * 4
         total = total + k(j) * x(i + j - 1)
      end do
      y(i) = total
   end do

end subroutine

 !> Return the smallest multiply of pad_modulo which is greater
 !> or equal to kernel_size. To disable padding, put pad_modulo=1.
elemental function padded_dimension(kernel_size, pad_modulo)
   !> Positive integer.
   integer(kind=size_k), intent(in) :: kernel_size
   !> Positive integer. For pad_modulo=1, same dimension is returned.
   integer(kind=size_k), intent(in) :: pad_modulo
   !> Dimension after padding.
   integer(kind=size_k) :: padded_dimension

   if (pad_modulo <= 1) then
      padded_dimension = kernel_size
      return
   end if

   padded_dimension = kernel_size + modulo(-kernel_size, pad_modulo)

end function

 !> Invert and pad 1D kernel with zeros, so that its length is a multiply of pad_modulo.
 !> For pad_modulo=1, no padding is performed.
pure function padded_1d_kernel(k, pad_modulo) result(padded_kernel)
   !> Kernel array.
   real(real_k), intent(in) :: k(:)
   !> Padding modulo. Put 1 to disable padding.
   integer(kind=size_k), intent(in) :: pad_modulo
   !> Padded and inverted kernel.
   real(kind=real_k) :: padded_kernel(padded_dimension(size(k, kind=size_k), pad_modulo))

   integer(kind=size_k) :: kernel_size, padded_size

   kernel_size = size(k, kind=size_k)
   padded_size = padded_dimension(kernel_size, pad_modulo)

   padded_kernel(1:kernel_size) = k(kernel_size:1:-1)
   if (padded_size > kernel_size) padded_kernel(kernel_size + 1 :) = 0

end function

 !> Compute convolution for kernel which is padded with zeros.
 !> This is good for performance reasons, as such kernels can
 !> be better vectorized into SIMD instructions.
pure subroutine conv1d_pad_core(x, k, kernel_size, y)
   !> Input vector.
   real(real_k), intent(in), contiguous :: x(:)
   !> Reversed kernel padded with zeros.
   real(real_k), intent(in), contiguous :: k(:)
   !> Width of the padding. For zero this will be a normal convolution.
   integer(size_k), intent(in) :: kernel_size
   !> Output.
   real(real_k), intent(out), contiguous :: y(:)

   integer(size_k) :: input_size, padding, output_size_raw

   input_size = size(x, kind=size_k)
   padding = size(k, kind=size_k) - kernel_size
   output_size_raw = input_size - kernel_size + 1_size_k

#       ifndef NDEBUG
   if (padding < 0) error stop "conv1d_pad_core: padding parameter must be >= 0"
   if (size(y) /= output_size_raw) &
      error stop 'incorrect output shape for 1D convolution'
#       endif

   call conv1d_core(x, k, y(:output_size_raw - padding))

   if (padding > 0) then
      call conv1d_core(x(output_size_raw - padding + 1_size_k:), &
         k(:kernel_size), &
         y(output_size_raw - (padding - 1_size_k) : output_size_raw))
   end if

end subroutine

 !> Compute convolution for kernel which is padded with zeros.
 !> This is good for performance reasons, as such kernels can
 !> be better vectorized into SIMD instructions.
pure subroutine conv1d_pad(x, k, kernel_size, keep_shape, y)
   !> Input vector.
   real(real_k), intent(in), contiguous :: x(:)
   !> Reversed kernel padded with zeros.
   real(real_k), intent(in), contiguous :: k(:)
   !> Width of the padding. For zero this will be a normal convolution.
   integer(size_k), intent(in) :: kernel_size
   !> Whether to keep the same output dimensions as input.
   logical, intent(in) :: keep_shape
   !> Output.
   real(real_k), intent(out), contiguous :: y(:)

   integer(size_k) :: expected_output_shape, output_shape_raw, output_offset, input_shape

   input_shape = size(x)
   output_shape_raw = input_shape + 1_size_k - kernel_size
   expected_output_shape = input_shape + merge(0_size_k, 1_size_k - kernel_size, keep_shape)
   output_offset = merge((kernel_size - 1_size_k) / 2_size_k, 0_size_k, keep_shape)

#   ifndef NDEBUG
   if (size(y) /= expected_output_shape) then
      block
         character(len=256) :: errmsg
         write (errmsg, '(a, a, i0, ", ", i0, a, i0, ", ", i0, a)') &
            "incorrect shape for 1D convolution output; ", &
            "expected [", expected_output_shape, "], but got [", size(y), "]"
         error stop trim(errmsg)
      end block
   end if
#   endif

   call conv1d_pad_core(x, k, kernel_size, y(1+output_offset:output_shape_raw+output_offset))

end subroutine

!> Invert and pad 2D kernel with zeros, in the first dimension,
!> so that its length is a multiply of pad_modulo.
!> For pad_modulo=1, no padding is performed.
!> Padding higher dimensions does not bring any benefit from the
!> performance standpoint.
pure function padded_2d_kernel(k, pad_modulo) result(padded_kernel)
   !> Kernel array.
   real(real_k), intent(in) :: k(:,:)
   !> Padding modulo. Put 1 to disable padding.
   integer(kind=size_k), intent(in) :: pad_modulo
   !> Padded and inverted kernel.
   real(kind=real_k) :: padded_kernel(&
      padded_dimension(size(k, 1, kind=size_k), pad_modulo), &
      size(k, 2, kind=size_k))

   integer(kind=size_k) :: kernel_size(2), padded_size(2), j

   kernel_size = shape(k, kind=size_k)
   padded_size(1) = padded_dimension(kernel_size(1), pad_modulo)
   padded_size(2) = kernel_size(2)

   do j = 1, kernel_size(2)
      padded_kernel(1:kernel_size(1), j) = k(kernel_size(1):1:-1, kernel_size(2) - j + 1)
   end do
   if (padded_size(1) > kernel_size(1)) padded_kernel(kernel_size(1) + 1 :, :) = 0

end function



 !> Compute convolution for kernel which is padded with zeros.
 !> This is good for performance reasons, as such kernels can
 !> be better vectorized into SIMD instructions.
pure subroutine conv2d_pad(x, k, kernel_dim_1, keep_shape, y)
   !> Input vector.
   real(real_k), intent(in), contiguous :: x(:,:)
   !> Reversed kernel padded with zeros.
   real(real_k), intent(in), contiguous :: k(:,:)
   !> Width of the padding. For zero this will be a normal convolution.
   integer(size_k), intent(in) :: kernel_dim_1
   !> Whether to keep the same output dimensions as input.
   logical, intent(in) :: keep_shape
   !> Output.
   real(real_k), intent(out), contiguous :: y(:,:)

   real(real_k), allocatable :: buf(:)
   integer(size_k) :: expected_output_shape(2), output_shape_raw(2), output_offset(2), &
      input_shape(2), kernel_shape(2), ix, ik

   input_shape = shape(x)
   kernel_shape = [kernel_dim_1, size(k, 2, size_k)]
   output_shape_raw = input_shape + 1_size_k - kernel_shape
   expected_output_shape = input_shape + merge(0_size_k, 1_size_k - kernel_shape, keep_shape)
   output_offset = merge((kernel_shape - 1_size_k) / 2_size_k, 0_size_k, keep_shape)

#   ifndef NDEBUG
   if (any(shape(y) /= expected_output_shape)) then
      block
         character(len=256) :: errmsg
         write (errmsg, '(a, a, i0, ", ", i0, a, i0, ", ", i0, a)') &
            "incorrect shape for 2D convolution output; ", &
            "expected [", expected_output_shape, "], but got [", shape(y), "]"
         error stop trim(errmsg)
      end block
   end if
#   endif

   allocate(buf(output_shape_raw(1)))

   do ix = 1, output_shape_raw(2)
      associate(y_row => y( &
         1 + output_offset(1) : output_shape_raw(1) + output_offset(1), &
         ix + output_offset(2)))
         do ik = 1, kernel_shape(2)
            call conv1d_pad_core(x(:, ix + ik - 1), k(:,ik), kernel_dim_1, buf)
            if (ik == 1) then
               y_row(:) = buf
            else
               y_row(:) = y_row + buf
            end if
         end do
      end associate
   end do

end subroutine

subroutine conv2d_ref(x, k, keep_shape, y)
   real(real_k), intent(in), contiguous :: x(:,:), k(:,:)
   real(real_k), intent(inout), contiguous :: y(:,:)
   logical, intent(in) :: keep_shape

   integer(kind=size_k) :: ix, ik, jx, jk, input_shape(2), kernel_shape(2)
   integer(kind=size_k) :: output_shape_raw(2), expected_output_shape(2), output_offset(2)
   real(real_k) :: total

   input_shape = shape(x)
   kernel_shape = shape(k)
   output_shape_raw = input_shape + 1_size_k - kernel_shape
   expected_output_shape = input_shape + merge(0_size_k, 1_size_k - kernel_shape, keep_shape)
   output_offset = merge((kernel_shape - 1_size_k) / 2_size_k, 0_size_k, keep_shape)

#       ifndef NDEBUG
   if (any(shape(y) /= expected_output_shape)) then
      block
         character(len=256) :: errmsg
         write(errmsg, '(a, a, i0, ", ", i0, a, i0, ", ", i0, a)') &
            "incorrect shape for 2D convolution output; ", &
            "expected [", expected_output_shape, "], but got [", shape(y), "]"
         error stop trim(errmsg)
      end block
   end if
#       endif

   do jx = 1, output_shape_raw(2)
      do ix = 1, output_shape_raw(1)
         total = 0
         do jk = 1, kernel_shape(2)
            do ik = 1, kernel_shape(1)
               total = total + k(kernel_shape(1) + 1 - ik, kernel_shape(2) + 1 - jk) &
                  * x(ix + ik - 1, jx + jk - 1)
            end do
         end do
         y(ix + output_offset(1), jx + output_offset(2)) = total
      end do
   end do

end subroutine

end module
