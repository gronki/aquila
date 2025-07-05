module projection_m

use findstar, only: xy_to_ij, ij_to_xy
use globals
use transform_m
use transform_xyr_m
implicit none (type, external)
public

contains
 !------------------------------------------------------------------------------------!

subroutine project_bilinear(v, im0, im, resample)
   class(transform_t), intent(in) :: v
   real(fp), dimension(:,:), intent(in), contiguous :: im0
   real(fp), dimension(:,:), intent(out), contiguous :: im
   real(fp), intent(in), optional :: resample

   integer   ::  i,  j, ni, nj, i0, j0, ni0, nj0
   integer   :: ki, kj
   real(fp)  :: i0f, j0f, ri, rj, i1, j1, scale, x, y, x0, y0

   ni = size(im, 1)
   nj = size(im, 2)
   ni0 = size(im0, 1)
   nj0 = size(im0, 2)

   scale = 1
   if (present(resample)) scale = resample

   do j = 1, size(im, 2)
      do i = 1, size(im, 1)

         call ij_to_xy(real(i, fp), real(j, fp), ni, nj, scale, x, y)
         call v % apply(x, y, x0, y0)
         call xy_to_ij(x0, y0, ni0, nj0, real(1, fp), i0f, j0f)

         i0 = floor(i0f)
         j0 = floor(j0f)

         ki = max(1, min(i0, ni0 - 1))
         kj = max(1, min(j0, nj0 - 1))

         ri = i0f - ki
         rj = j0f - kj

         if ( abs(2 * ri - 1) <= 2 .and. abs(2 * rj - 1) <= 2 ) then
            im(i,j)  = im0(ki,   kj  ) * (1 - ri)  * (1 - rj)  &
            &        + im0(ki+1, kj  ) * ri        * (1 - rj)  &
            &        + im0(ki,   kj+1) * (1 - ri)  * rj        &
            &        + im0(ki+1, kj+1) * ri        * rj
         end if
      end do
   end do

end subroutine
end module

