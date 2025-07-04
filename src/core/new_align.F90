module new_align

  use globals
  use findstar, only: source_t, xy_to_ij, ij_to_xy
  implicit none
  private
  public :: transform_t, transform_xyr_t, align_gravity, project_bilinear, align_polygon

  type, abstract :: transform_t
    real(fp) :: scale = 1
    real(fp), allocatable :: vec(:)
  contains
    procedure(iface_npar), deferred :: npar
    procedure(iface_apply), deferred :: apply
    procedure(iface_pder), deferred :: pder
  end type

  abstract interface

    elemental function iface_npar(t) result(npar)
      import transform_t
      class(transform_t), intent(in) :: t
      integer :: npar
    end function

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
    procedure :: npar   => xyr_npar
  end type

  interface transform_xyr_t
    module procedure transform_xyr_ctor
  end interface

  !------------------------------------------------------------------------------------!

contains

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

  elemental function xyr_npar(t) result(npar)
    class(transform_xyr_t), intent(in) :: t
    integer :: npar

    npar = 3
  end function

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
