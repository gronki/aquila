module projection_m

use source_m
use globals
use transform_m
use transform_xyr_m
implicit none (type, external)
public

contains

!------------------------------------------------------------------------------------!

pure function interpolate_point(i0f, j0f, ni0, nj0, im0) result(output)

   real(r64_k), intent(in) :: i0f, j0f
   integer(i64_k), intent(in) :: ni0, nj0
   real(buf_k), intent(in) :: im0(ni0, nj0)
   real(buf_k) :: output
   integer(i64_k)   :: ki, kj, i0, j0
   real(buf_k) :: ri, rj

   i0 = floor(i0f)
   j0 = floor(j0f)

   ki = max(1, min(i0, ni0 - 1))
   kj = max(1, min(j0, nj0 - 1))

   ri = real(i0f, buf_k) - ki
   rj = real(j0f, buf_k) - kj

   if ( abs(2 * ri - 1) <= 2 .and. abs(2 * rj - 1) <= 2 ) then
      output   = im0(ki,   kj  ) * (1 - ri)  * (1 - rj)  &
      &        + im0(ki+1, kj  ) * ri        * (1 - rj)  &
      &        + im0(ki,   kj+1) * (1 - ri)  * rj        &
      &        + im0(ki+1, kj+1) * ri        * rj
   end if

end function

!------------------------------------------------------------------------------------!

subroutine project_bilinear(v, im0, im, resample)
   class(transform_t), intent(in) :: v
   real(buf_k), dimension(:,:), intent(in), contiguous :: im0
   real(buf_k), dimension(:,:), intent(out), contiguous :: im
   real(r64_k), intent(in), optional :: resample

   integer(i64_k)   ::  i,  j, ni, nj, i0, j0, ni0, nj0
   real(r64_k) :: i0f, j0f, scale, x, y, x0, y0

   ni = size(im, 1)
   nj = size(im, 2)
   ni0 = size(im0, 1)
   nj0 = size(im0, 2)

   scale = 1
   if (present(resample)) scale = resample

   do j = 1, size(im, 2)
      do i = 1, size(im, 1)

         call ij_to_xy(real(i, r64_k), real(j, r64_k), ni, nj, scale, x, y)
         call v % apply(x, y, x0, y0)
         call xy_to_ij(x0, y0, ni0, nj0, real(1, r64_k), i0f, j0f)

         im(i,j) = interpolate_point(i0f, j0f, ni0, nj0, im0)

      end do
   end do

end subroutine

!------------------------------------------------------------------------------------!

subroutine project_bilinear_c(vc, im0, im, resample, err) bind(C, name="project_bilinear")
   use aquila_c_binding
   use transform_c_binding
   type(transform_c_t), intent(in) :: vc
   type(buffer_descriptor_r32_t), value :: im0
   type(buffer_descriptor_r32_t), value :: im
   real(buf_k), pointer, contiguous :: im0ptr(:,:), imptr(:,:)
   real(r64_k), intent(in), value :: resample
   class(transform_t), allocatable :: v
   type(error_status_t) :: err

   call reset_err(err)
   call c_to_f_transform(vc, v, err)
   if (check_err(err)) return

   im0ptr => from_descriptor(im0)
   imptr => from_descriptor(im)

   call project_bilinear(v, im0ptr, imptr, resample)

end subroutine

end module

