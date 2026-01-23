module alignment_m

use source_m, only: source_t
use globals
use transform_m
use transform_xyr_m
use transform_affine_m
implicit none (type, external)
public

type, bind(C) :: align_params_t
   real(r64_k) :: scale = 1
   real(r64_k) :: gravity_precision = 2
   real(r64_k) :: gravity_precision_pre = 10
   integer(i32_k) :: poly_stars = 32
   integer(i32_k) :: poly_matches = 16
   logical(c_bool) :: prealign_polygon = .false.
end type

contains

 !------------------------------------------------------------------------------------!

function default_align_params() bind(C)
   type(align_params_t) :: default_align_params
end function

 !------------------------------------------------------------------------------------!

subroutine align_polygon(xy1, xy2, nstars, nmatches, t)
   use polygon_matching, only: find_transform_polygons
   class(transform_xyr_t), intent(inout) :: t
   type(source_t), intent(in) :: xy1(:), xy2(:)
   integer, intent(in) :: nstars, nmatches
   real(r64_k) :: init_dx, init_dy, init_r

   call find_transform_polygons(xy1, xy2, nstars, nmatches, init_dx, init_dy, init_r)
   t%vec(:3) = [init_dx, init_dy, init_r * t%scale]
end subroutine

 !------------------------------------------------------------------------------------!

subroutine classic_align(lst0, lst, align_method, params, tx, errno, verbose)

   type(source_t), intent(in) :: lst0(:), lst(:)
   character(len=*), intent(in) :: align_method
   class(transform_t), intent(out), allocatable :: tx
   type(align_params_t), intent(in) :: params
   integer, intent(out) :: errno
   logical, intent(in), optional :: verbose

   real(r64_k) :: a
   type(transform_xyr_t), allocatable :: xyr

   xyr = transform_xyr_t(params%scale)

   if (params % prealign_polygon) then
      call align_polygon(lst0, lst, params%poly_stars, params%poly_matches, xyr)
   end if

   errno = 0
   select case (align_method)

   case ('polygon')
      if (.not. params % prealign_polygon) &
         call align_polygon(lst0, lst, params%poly_stars, params%poly_matches, xyr)
      call move_alloc(xyr, tx)

   case ('xyr')
      ! find an initial estimate for a transform
      ! fine-tune the transform between frames
      if (.not. params % prealign_polygon) &
         call align_gravity(lst0, lst, xyr, params%gravity_precision_pre, verbose)
      call align_gravity(lst0, lst, xyr, params%gravity_precision, verbose)
      call move_alloc(xyr, tx)

   case ('affine')
      block
         type(transform_affine_t), allocatable :: aff
         ! find an initial estimate for a transform
         if (.not. params % prealign_polygon) &
            call align_gravity(lst0, lst, xyr, params%gravity_precision_pre, verbose)
         ! initialize the affine transform from that
         call align_gravity(lst0, lst, xyr, params%gravity_precision, verbose)
         aff = affine_from_xyr(xyr)
         ! fine-tune
         call align_gravity(lst0, lst, aff, params%gravity_precision, verbose)
         call move_alloc(aff, tx)
      end block

   case default
      print *, 'align: unknown method: ', align_method
      errno = 1
   end select
end subroutine

 !------------------------------------------------------------------------------------!

subroutine align_gravity(xy, xy0, v0, k0, verbose)
   type(source_t), intent(in) :: xy(:), xy0(:)
   !> transform to be found
   class(transform_t), intent(inout) :: v0
   !> radius of the potential decay
   real(r64_k), intent(in) :: k0
   !> print debug?
   logical, intent(in), optional :: verbose
   logical :: verbose_

   type(source_t), allocatable :: xy1(:)
   integer :: ii, npar
   real(r64_k) :: y0, lam
   real(r64_k) :: y0_dv(v0%npar()), y0n_dv(v0%npar())
   ! maximum dF/dx at x = k0 / sqrt(2)

   verbose_ = .false.; if (present(verbose)) verbose_ = verbose

   npar = v0%npar()

   allocate(xy1(size(xy)))

   if (verbose_) write (0, '("k0 =", g10.4)') k0
   if (verbose_) write (0, '(a4,  a9  , 3a9  )') &
   &     'ii', 'lam', 'vec(1)', 'vec(2)', '...'

   iter_convergence: do ii = 1, 20

      if (verbose_) write (0,*) '   *** ITERATION ', ii, ' ***'
      ! compute the gradient
      call comp_ydv(v0, y0, y0_dv)
      ! normalize the gradient vector length to 1
      y0n_dv = y0_dv / sqrt(sum(y0_dv**2))

      lam = -0.01

      call minimize_along_vec(lam, 0.25 * k0)
      v0 % vec(:npar) = v0 % vec(:npar) + y0n_dv * lam

      if (verbose_) write (0, '(i4, f9.4, *(f8.3))') ii, lam, v0 % vec(:npar)

      if ( lam .lt. 0.005 ) then
         if (verbose_) write (0,*) ' ---- precision reached'
         return
      end if

      if (verbose_) write (0,*) '---------------------------------'
   end do iter_convergence

contains

   subroutine minimize_along_vec(x, dx_0)
      real(r64_k), intent(inout) :: x
      real(r64_k), intent(in) :: dx_0
      integer :: i, ii
      real(r64_k) :: dx
      real(r64_k) :: y, y_dx, y_dv(npar)
      class(transform_t), allocatable :: v
      integer, parameter :: u = 8

      allocate(v, source=v0)
      dx = dx_0

      if (verbose_) write (0, '(2A3,A15  , a16, a12 , a20 )') 'ii', 'i', 'x', 'y', 'y_dx', 'vec ...'

      loop_scales: do ii = 1, 7
         scan_interval: do i = 1, u
            x = x + dx

            v % vec(:npar) = v0 % vec(:npar) + y0n_dv * x
            call comp_ydv(v, y, y_dv)
            y_dx = sum(y_dv * y0n_dv)

            if (verbose_) write (0, '(2I3,F15.8,es16.8,es12.4,*(f10.3))') ii, i, x, y, y_dx, v%vec(:npar)

            if (y_dx < 0) then
               if (verbose_) write (0, *) '             < < <'
               x = x - dx
               dx = dx / u
               exit scan_interval
            end if
         end do scan_interval
      end do loop_scales

      if (verbose_) write (0, *) '  ================='

   end subroutine

   subroutine comp_ydv(v, y, y_dv)

      class(transform_t), intent(in) :: v
      real(r64_k), intent(out) :: y, y_dv(npar)
      real(r64_k) :: aa, bb, y_dx1, y_dy1
      real(r64_k), dimension(npar) :: x1_dv, y1_dv
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
            bb = (xy0(i0) % flux * xy(i) % flux)**0.25_r64_k * k0
            y = y + bb / aa
            y_dx1 = - (xy1(i) % x - xy0(i0) % x) * bb / aa**3
            y_dy1 = - (xy1(i) % y - xy0(i0) % y) * bb / aa**3
            y_dv(:) = y_dv(:) + y_dx1 * x1_dv(:) + y_dy1 * y1_dv(:)
         end do
      end do
   end subroutine

end subroutine

end module
