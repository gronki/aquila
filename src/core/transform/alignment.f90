module alignment_m

use findstar, only: source_t
use globals
use transform_m
use transform_xyr_m
implicit none (type, external)
public

contains
 !------------------------------------------------------------------------------------!

subroutine align_polygon(xy1, xy2, nstars, nmatches, t)
   use polygon_matching, only: find_transform_polygons
   class(transform_xyr_t), intent(inout) :: t
   class(source_t), intent(in) :: xy1(:), xy2(:)
   integer, intent(in) :: nstars, nmatches
   real(fp) :: init_dx, init_dy, init_r

   call find_transform_polygons(xy1, xy2, nstars, nmatches, init_dx, init_dy, init_r)
   t%vec = [init_dx, init_dy, init_r * t%scale]
end subroutine

 !------------------------------------------------------------------------------------!

subroutine align_gravity(xy, xy0, v0, k0)
   class(source_t), intent(in) :: xy(:), xy0(:)
   class(transform_t), intent(inout) :: v0
   type(source_t) :: xy1(size(xy))
   integer :: ii, nmax, npar
   real(fp) :: k0, y0, lam
   real(fp) :: y0_dv(v0%npar()), y0n_dv(v0%npar())
   ! maximum dF/dx at x = k0 / sqrt(2)

   if (.not. allocated(v0 % vec)) then
      error stop "fatal: transform v0 not initialized"
   end if

   npar = v0%npar()

   ! allocate(y0_dv(npar), y0n_dv(npar))

#   ifdef _DEBUG
   write (0, '("k0 =", g10.4)') k0
   write (0, '(a4, a7  , a9  , 3a9  )') &
   &     'ii', 'k0', 'lam', 'vec(1)', 'vec(2)', '...'
#   endif

   loop_star_sharpness: do ii = 1, 10

      ! compute the gradient
      call comp_ydv(v0, y0, y0_dv)
      ! normalize the gradient vector length to 1
      y0n_dv = y0_dv / sqrt(sum(y0_dv**2))

      lam = -0.01

      call minimize_along_vec(lam, 0.25 * k0)
      v0 % vec = v0 % vec + y0n_dv * lam

#     ifdef _DEBUG
      write (0, '(i4, f7.2, f9.4, *(f9.4))') ii, k0, lam, v0 % vec
#     endif

      if ( lam .lt. 0.005 ) then
#       ifdef _DEBUG
         write (0,*) ' ---- precision reached'
#       endif
         exit loop_star_sharpness
      end if

      ! k0 = k0 * k0decr

      ! write (0,*) '---------------------------------'
   end do loop_star_sharpness

contains

   subroutine minimize_along_vec(x, dx_0)
      real(fp), intent(inout) :: x
      real(fp), intent(in) :: dx_0
      integer :: i, ii
      real(fp) :: dx
      real(fp) :: y, y_dx, y_dv(npar)
      class(transform_t), allocatable :: v
      integer, parameter :: u = 8

      allocate(v, source=v0)
      dx = dx_0

#     ifdef _DEBUG
      ! write (0, '(2A3,A15  , a16, a12 , a20 )') 'ii', 'i', 'x', 'y', 'y_dx', 'vec(n) ...'
#     endif

      loop_scales: do ii = 1, 7
         scan_interval: do i = 1, u
            x = x + dx

            v % vec = v0 % vec + y0n_dv * x
            call comp_ydv(v, y, y_dv)
            y_dx = sum(y_dv * y0n_dv)

#         ifdef _DEBUG
            ! write (0, '(2I3,F15.8,es16.8,es12.4,*(f10.3))') ii, i, x, y, y_dx, v%vec(1:v%npar())
#         endif

            if (y_dx < 0) then
#           ifdef _DEBUG
               ! write (0, *) '             < < <'
#           endif
               x = x - dx
               dx = dx / u
               exit scan_interval
            end if
         end do scan_interval
      end do loop_scales

#     ifdef _DEBUG
      ! write (0, *) '  ================='
#     endif

   end subroutine

   subroutine comp_ydv(v, y, y_dv)

      class(transform_t), intent(in) :: v
      real(fp), intent(out) :: y, y_dv(npar)
      real(fp) :: aa, bb, y_dx1, y_dy1
      real(fp), dimension(npar) :: x1_dv, y1_dv
      integer :: i0, i

      y = 0
      y_dv(:) = 0

      call v % apply(xy % x, xy % y, xy1 % x, xy1 % y)
      xy1 % flux = xy % flux

      do i = 1, size(xy)
         call v % pder(xy(i) % x, xy(i) % y, x1_dv(:), y1_dv(:))
         do i0 = 1, size(xy0)
            aa = sqrt((xy1(i) % x - xy0(i0) % x)**2 &
            &   + (xy1(i) % y - xy0(i0) % y)**2 + k0**2)
            bb = (xy0(i0) % flux * xy(i) % flux)**0.25_fp * k0
            y = y + bb / aa
            y_dx1 = - (xy1(i) % x - xy0(i0) % x) * bb / aa**3
            y_dy1 = - (xy1(i) % y - xy0(i0) % y) * bb / aa**3
            y_dv(:) = y_dv(:) + y_dx1 * x1_dv(:) + y_dy1 * y1_dv(:)
         end do
      end do
   end subroutine

end subroutine

end module
