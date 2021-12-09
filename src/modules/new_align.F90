module new_align

  use globals
  use findstar, only: source
  implicit none
  private
  public :: transform_t, transform_xyr_t, align2, improject2

  type, abstract :: transform_t
    real(fp) :: scale = 1
    real(fp), allocatable :: vec(:)
  contains
    procedure :: npar
    procedure(iface_apply), deferred :: apply
    procedure(iface_pder), deferred :: pder
    procedure, pass(v) :: project => improject2
    procedure, pass(v0) :: align => align2
  end type

  abstract interface

    elemental subroutine iface_apply(t, x1, y1, x2, y2)
      import transform_t, fp
      class(transform_t), intent(in) :: t
      real(fp), intent(in) :: x1, y1
      real(fp), intent(out) :: x2, y2
    end subroutine

    pure subroutine iface_pder(t, x, y, dx, dy)
      import transform_t, fp
      class(transform_t), intent(in) :: t
      real(fp), intent(in) :: x, y
      real(fp), dimension(:), intent(out) :: dx, dy
    end subroutine

  end interface

  !------------------------------------------------------------------------------------!

  type, extends(transform_t) :: transform_xyr_t
  contains
    procedure :: apply  => xyr_apply
    procedure :: pder   => xyr_pder
    procedure :: align_polygon
  end type

  interface transform_xyr_t
    module procedure transform_xyr_ctor
  end interface

  !------------------------------------------------------------------------------------!

contains

  !------------------------------------------------------------------------------------!

  elemental function npar(t)
    class(transform_t), intent(in) :: t
    integer :: npar

    if (allocated(t % vec)) then
      npar = size(t % vec)
    else
      npar = 0
    end if
  end function

  !------------------------------------------------------------------------------------!

  function transform_xyr_ctor(scale) result(self)
    type(transform_xyr_t) :: self
    real(fp), intent(in), optional :: scale

    allocate(self % vec(3))
    self % vec(:) = 0

    if (present(scale)) self%scale = scale
  end function


  elemental subroutine xyr_apply(t, x1, y1, x2, y2)
    class(transform_xyr_t), intent(in) :: t
    real(fp), intent(in) :: x1, y1
    real(fp), intent(out) :: x2, y2
    real(fp) :: th

    th = t % vec(3) / t % scale
    x2 = t % vec(1) + cos(th) * x1 - sin(th) * y1
    y2 = t % vec(2) + sin(th) * x1 + cos(th) * y1
  end subroutine


  pure subroutine xyr_pder(t, x, y, dx, dy)
    class(transform_xyr_t), intent(in) :: t
    real(fp), intent(in) :: x, y
    real(fp), dimension(:), intent(out) :: dx, dy
    real(fp) :: th

    dx(1) = 1
    dy(1) = 0
    dx(2) = 0
    dy(2) = 1

    th = t % vec(3) / t % scale
    dx(3) = (- sin(th) * x - cos(th) * y) / (t % scale)
    dy(3) = (  cos(th) * x - sin(th) * y) / (t % scale)
  end subroutine


  subroutine align_polygon(t, xy1, xy2, nstars, nmatches)
    use polygon_matching, only: find_transform_polygons
    class(transform_xyr_t), intent(inout) :: t
    class(source), intent(in) :: xy1(:), xy2(:)
    integer, intent(in) :: nstars, nmatches
    real(fp) :: init_dx, init_dy, init_r

    call find_transform_polygons(xy1, xy2, nstars, nmatches, init_dx, init_dy, init_r)
    t%vec(1:3) = [init_dx, init_dy, init_r * t%scale]
  end subroutine

  !------------------------------------------------------------------------------------!

  subroutine align2(xy, xy0, v0, k0)
    class(source), intent(in) :: xy(:), xy0(:)
    class(transform_t), intent(inout) :: v0
    type(source) :: xy1(size(xy))
    integer :: ii, nmax
    real(fp) :: k0, y0, lam
    real(fp) :: y0_dv(size(v0%vec)), y0n_dv(size(v0%vec))
    ! maximum dF/dx at x = k0 / sqrt(2)

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
      real(fp) :: y, y_dv(size(v0%vec)), y_dx
      class(transform_t), allocatable :: v
      integer, parameter :: u = 8

      v = v0
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
      real(fp), intent(out) :: y, y_dv(size(v0%vec))
      real(fp) :: aa, bb, y_dx1, y_dy1
      real(fp), dimension(size(v0%vec)) :: x1_dv, y1_dv
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

  !------------------------------------------------------------------------------------!

  subroutine improject2(v, im0, im, resample)
    class(transform_t), intent(in) :: v
    real(fp), dimension(:,:), intent(in), contiguous :: im0
    real(fp), dimension(:,:), intent(out), contiguous :: im
    real(fp), intent(in), optional :: resample

    integer   ::  i,  j
    integer   :: ki, kj
    real(fp)  :: xi, xj, ri, rj, i1, j1, ci, cj

    ci = 0.5_fp * (size(im0, 1) + 1)
    cj = 0.5_fp * (size(im0, 2) + 1)

    do j = 1, size(im, 2)
      do i = 1, size(im, 1)

        if ( present(resample) ) then
          i1 = (i - 0.5_fp) / resample + 0.5_fp
          j1 = (j - 0.5_fp) / resample + 0.5_fp
        else
          i1 = i; j1 = j
        end if

        call v % apply(i1 - ci, j1 - cj, xi, xj)

        xi = xi + ci
        xj = xj + cj

        ki = max(1, min(floor(xi), size(im0, 1) - 1))
        kj = max(1, min(floor(xj), size(im0, 2) - 1))

        ri = xi - ki
        rj = xj - kj

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
