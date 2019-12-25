module new_align

  use globals
  use findstar, only: source
  implicit none
  private
  public :: transform_t, transform_xyr_t, transform_vec_size, align2, improject2

  integer, parameter :: transform_vec_size = 8

  type, abstract :: transform_t
    real(fp) :: r0 = 1
    real(fp) :: vec(transform_vec_size) = 0
  contains
    procedure(iface_npar), deferred :: npar
    procedure(iface_det), deferred :: det
    procedure(iface_apply), deferred :: apply
    procedure(iface_pder), deferred :: pder
    procedure, pass(v) :: project => improject2
    procedure, pass(v0) :: align => align2
    ! procedure(iface_getvec), deferred :: vec
    ! procedure(iface_setvec), deferred, private :: setvec
    ! generic :: operator(=) => setvec
  end type

  abstract interface
    elemental function iface_npar(t) result(npar)
      import transform_t, fp
      class(transform_t), intent(in) :: t
      integer :: npar
    end function
    ! pure function iface_getvec(t) result(vec)
    !   import transform_t, fp
    !   class(transform_t), intent(in) :: t
    !   real(fp), allocatable :: vec(:)
    ! end function
    elemental function iface_det(t) result(det)
      import transform_t, fp
      class(transform_t), intent(in) :: t
      real(fp) :: det
    end function
    elemental subroutine iface_apply(t, x1, y1, x2, y2)
      import transform_t, fp
      class(transform_t), intent(in) :: t
      real(fp), intent(in) :: x1, y1
      real(fp), intent(out) :: x2, y2
    end subroutine
    pure subroutine iface_pder(t, x, y, dx, dy)
      import transform_t, fp, transform_vec_size
      class(transform_t), intent(in) :: t
      real(fp), intent(in) :: x, y
      real(fp), dimension(transform_vec_size), intent(out) :: dx, dy
    end subroutine
    ! pure subroutine iface_setvec(t, v)
    !   import transform_t, fp
    !   class(transform_t), intent(inout) :: t
    !   real(fp), intent(in) :: v(:)
    ! end subroutine
  end interface

  !------------------------------------------------------------------------------------!

  type, extends(transform_t) :: transform_xyr_t
    ! real(fp) :: x, y, r
  contains
    procedure :: npar   => xyr_npar
    procedure :: det    => xyr_det
    procedure :: apply  => xyr_apply
    procedure :: pder   => xyr_pder
    ! procedure :: vec    => xyr_vec
    ! procedure :: setvec => xyr_setvec
  end type

  !------------------------------------------------------------------------------------!

contains

  !------------------------------------------------------------------------------------!

  elemental function xyr_npar(t) result(npar)
    class(transform_xyr_t), intent(in) :: t
    integer :: npar
    npar = 3
  end function

  ! pure function xyr_vec(t) result(vec)
  !   class(transform_xyr_t), intent(in) :: t
  !   real(fp), allocatable :: vec(:)
  !   allocate(vec(3))
  !   vec(1) = t % x; vec(2) = t % y; vec(3) = t % r
  ! end function

  elemental function xyr_det(t) result(det)
    class(transform_xyr_t), intent(in) :: t
    real(fp) :: det
    det = 1
  end function

  elemental subroutine xyr_apply(t, x1, y1, x2, y2)
    class(transform_xyr_t), intent(in) :: t
    real(fp), intent(in) :: x1, y1
    real(fp), intent(out) :: x2, y2
    associate (th => t % vec(3) / t % r0)
      x2 = t % vec(1) + cos(th) * x1 - sin(th) * y1
      y2 = t % vec(2) + sin(th) * x1 + cos(th) * y1
    end associate
  end subroutine

  pure subroutine xyr_pder(t, x, y, dx, dy)
    class(transform_xyr_t), intent(in) :: t
    real(fp), intent(in) :: x, y
    real(fp), dimension(transform_vec_size), intent(out) :: dx, dy
    dx(1) = 1
    dy(1) = 0
    dx(2) = 0
    dy(2) = 1
    associate (th => t % vec(3) / t % r0)
      dx(3) = (- sin(th) * x - cos(th) * y) / (t % r0)
      dy(3) = (  cos(th) * x - sin(th) * y) / (t % r0)
    end associate
    dx(4:) = 0; dy(4:) = 0
  end subroutine

  ! pure subroutine xyr_setvec(t, v)
  !   class(transform_xyr_t), intent(inout) :: t
  !   real(fp), intent(in) :: v(:)
  !   if (size(v) /= 3) error stop
  !   t % x = v(1); t % y = v(2); t % r = v(3)
  ! end subroutine

  !------------------------------------------------------------------------------------!

  subroutine align2(xy, xy0, v0)
    class(source), intent(in) :: xy(:)
    class(source), intent(in) :: xy0(:)
    class(transform_t), intent(inout) :: v0
    type(source) :: xy1(size(xy))
    integer :: ii, nmax
    real(fp) :: k0, y0, lam
    real(fp) :: y0_dv(transform_vec_size), y0n_dv(transform_vec_size)
    ! maximum dF/dx at x = k0 / sqrt(2)
    real(fp), parameter :: k0min = 1.4, k0decr = 0.75

    k0 = 0.5 * (v0 % r0) / sqrt(real(size(xy0)))
    nmax = ceiling(log(k0min / k0) / log(k0decr))
    nmax = max(nmax, 3)
    k0 = k0min / k0decr**(nmax - 1)

#   ifdef _DEBUG
    write (0, '("k0 =", g10.4)') k0
    write (0, '(a4, a7  , a9  , 3a9  )') &
    &     'ii', 'k0', 'lam', 'vec(1)', 'vec(2)', '...'
#   endif

    loop_star_sharpness: do ii = 1, nmax

      ! compute the gradient
      call comp_ydv(v0, y0, y0_dv)
      ! normalize the gradient vector length to 1
      y0n_dv = y0_dv / sqrt(sum(y0_dv**2))

      lam = -0.01

      call minimize_along_vec(lam, 0.25 * k0)
      v0 % vec = v0 % vec + y0n_dv * lam

#     ifdef _DEBUG
      write (0, '(i4, f7.2, f9.4, *(f9.4))') ii, k0, lam, v0 % vec(1:v0 % npar())
#     endif

      if ( lam .lt. 0.005 ) then
#       ifdef _DEBUG
        write (0,*) ' ---- precision reached'
#       endif
        exit loop_star_sharpness
      end if

      k0 = k0 * k0decr

      ! write (0,*) '---------------------------------'
    end do loop_star_sharpness

  contains

    subroutine minimize_along_vec(x, dx_0)
      real(fp), intent(inout) :: x
      real(fp), intent(in) :: dx_0
      integer :: i, ii
      real(fp) :: dx
      real(fp) :: y, y_dv(transform_vec_size), y_dx
      class(transform_t), allocatable :: v

      v = v0
      dx = dx_0

      ! write (0, '(2A3,A20  ,2 A14  )') 'ii', 'i', 'x', 'y', 'y_dx'

      loop_scales: do ii = 1, 7
        scan_interval: do i = 1, 10
          x = x + dx

          v % vec = v0 % vec + y0n_dv * x
          call comp_ydv(v, y, y_dv)
          y_dx = sum(y_dv * y0n_dv)

          ! write (0, '(2I3,F20.8,2Es14.6)') ii, i, x, y, y_dx

          if (y_dx < 0) then
            ! write (0, *) '             < < <'
            x = x - dx
            dx = dx / 10
            exit scan_interval
          end if
        end do scan_interval
      end do loop_scales

      ! write (0, *) '  ================='


    end subroutine

    subroutine comp_ydv(v, y, y_dv)

      class(transform_t), intent(in) :: v
      real(fp), intent(out) :: y, y_dv(transform_vec_size)
      real(fp) :: aa, bb, y_dx1, y_dy1
      real(fp), dimension(transform_vec_size) :: x1_dv, y1_dv
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
          bb = (xy0(i0) % flux * xy(i) % flux)**0.33_fp * k0
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
