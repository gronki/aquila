module legacy_align

  use globals
  use transforms
  use findstar, only: extended_source, source
  implicit none

contains

  subroutine align_xyr(xy, xy0, mx)
    class(source), intent(in) :: xy(:)
    class(source), intent(in) :: xy0(:)
    type(source) :: xy1(size(xy))
    real(fp), intent(inout) :: mx(2,3)
    integer :: ii, i, nmax
    integer, parameter :: i_x = 1, i_y = 2, i_r = 3
    real(fp) :: k0, y0, y0_dv(3), y0n_dv(3), v0(3)
    real(fp) :: lam, y_dlam, len0, r0
    real(fp) :: x0m, y0m
    real(fp), parameter :: k0min = 0.7, k0decr = 0.933

    x0m = sum(xy0 % x) / size(xy0)
    y0m = sum(xy0 % y) / size(xy0)

    r0 = 0.9 * maxval(sqrt((xy0 % x - x0m)**2 + (xy0 % y - y0m)**2))
    k0 = 0.25 * r0 / sqrt(real(size(xy0)))
    nmax = ceiling(log(k0min / k0) / log(k0decr))

    write (0, '("k0 =", g10.4)') k0
    write (0, '("r0 =", g10.4)') r0
    write (0, '("nmax =", g10.4)') nmax

    v0 = 0

    write (0, '(a4, a7  , a9  , 3a9  )') 'ii', 'k0', 'lam', 'dx', 'dy', 'rot'

    loop_star_sharpness: do ii = 1, nmax

      ! compute the gradient
      call comp_ydv(v0,y0,y0_dv)
      ! normalize the gradient vector length to 1
      y0n_dv = y0_dv / sqrt(sum(y0_dv**2))

      lam = -0.01

      call minimize_along_vec(lam, 1.0_fp)
      v0 = v0 + y0n_dv * lam

      write (0, '(i4, f7.2, f9.4, 3f9.4)') ii, k0, lam, v0

      if ( lam .lt. 1e-5 ) then
        write (0,*) ' ---- precision reached'
        exit loop_star_sharpness
      end if

      k0 = k0 * k0decr

      ! write (0,*) '---------------------------------'
    end do loop_star_sharpness

    call v2mx(v0, mx)

  contains

    subroutine minimize_along_vec(x,scale)
      real(fp), intent(inout) :: x
      real(fp), intent(in) :: scale
      integer :: i, ii
      real(fp) :: dx
      real(fp) :: v(3), y, y_dv(3), y_dx

      dx = scale

      ! write (0, '(2A3,A20  ,2 A14  )') 'ii', 'i', 'x', 'y', 'y_dx'

      loop_scales: do ii = 1, 10
        scan_interval: do i = 1, 10
          x = x + dx

          v = v0 + y0n_dv * x
          call comp_ydv(v, y, y_dv)
          y_dx = sum(y_dv * y0n_dv)

          ! write (0, '(2I3,F20.8,2Es14.6)') ii, i, x, y, y_dx

          if (y_dx < 0) then
            x = x - dx
            dx = dx / 10
            exit scan_interval
          end if
        end do scan_interval
      end do loop_scales

    end subroutine

    subroutine comp_ydv(v,y,y_dv)

      real(fp), intent(in) :: v(3)
      real(fp), intent(out) :: y, y_dv(3)
      real(fp) :: aa, bb, y_dx1, y_dy1, x1_dr, y1_dr
      integer :: i0, i

      y = 0
      y_dv = 0

      xy1 % x = v(i_x) + cos(v(i_r) / r0) * (xy % x) &
      &                - sin(v(i_r) / r0) * (xy % y)
      xy1 % y = v(i_y) + sin(v(i_r) / r0) * (xy % x) &
      &                + cos(v(i_r) / r0) * (xy % y)
      xy1 % flux = xy % flux

      do i0 = 1, size(xy0)
        do i = 1, size(xy)
          aa = sqrt((xy1(i) % x - xy0(i0) % x)**2 &
          &   + (xy1(i) % y - xy0(i0) % y)**2 + k0**2 )
          bb = sqrt(xy0(i0) % flux * xy(i) % flux) * k0
          y = y + bb / aa
          y_dx1 = - (xy1(i) % x - xy0(i0) % x) * bb / aa**3
          y_dy1 = - (xy1(i) % y - xy0(i0) % y) * bb / aa**3
          x1_dr = - (sin(v(i_r) / r0) * (xy(i) % x) &
          &       +  cos(v(i_r) / r0) * (xy(i) % y)) / r0
          y1_dr =   (cos(v(i_r) / r0) * (xy(i) % x) &
          &       -  sin(v(i_r) / r0) * (xy(i) % y)) / r0
          y_dv(i_x) = y_dv(i_x) + y_dx1
          y_dv(i_y) = y_dv(i_y) + y_dy1
          y_dv(i_r) = y_dv(i_r) + y_dx1 * x1_dr + y_dy1 * y1_dr
        end do
      end do
    end subroutine

    subroutine v2mx(v,mx)
      real(fp), intent(in) :: v(3)
      real(fp), intent(out) :: mx(2,3)
      mx = 0
      mx(1,1) =   v(i_x)
      mx(1,2) =   cos(v(i_r) / r0)
      mx(1,3) = - sin(v(i_r) / r0)
      mx(2,1) =   v(i_y)
      mx(2,2) =   sin(v(i_r) / r0)
      mx(2,3) =   cos(v(i_r) / r0)
    end subroutine

  end subroutine

  subroutine improject(im0, mx, im, resample)
    real(fp), dimension(:,:), intent(in) :: im0
    real(fp), dimension(:,:), intent(out) :: im
    real(fp), intent(in), optional :: resample

    real(fp) :: mx(2,3)
    integer   ::  i,  j
    integer   :: ki, kj
    real(fp)  :: xi, xj, ri, rj, i1, j1

    !$omp parallel do private(i, j, i1, j1, xi, xj, ki, kj, ri, rj)
    do j = 1, size(im, 2)
      do i = 1, size(im, 1)

        if ( present(resample) ) then
          i1 = (i - 0.5_fp) / resample + 0.5_fp
          j1 = (j - 0.5_fp) / resample + 0.5_fp
        else
          i1 = i; j1 = j
        end if

        xi = mx(1,1) + mx(1,2) * i1 + mx(1,3) * j1
        xj = mx(2,1) + mx(2,2) * i1 + mx(2,3) * j1

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
    !$omp end parallel do

  end subroutine

end module
