module legacy_align

  use globals
  use findstar, only: extended_source_t, source_t
  implicit none

contains

  subroutine align_xyr(xy, xy0, mx)
    class(source_t), intent(in) :: xy(:)
    class(source_t), intent(in) :: xy0(:)
    type(source_t) :: xy1(size(xy))
    real(fp), intent(inout) :: mx(2,3)
    integer :: ii, i, nmax
    integer, parameter :: i_x = 1, i_y = 2, i_r = 3
    real(fp) :: k0, y0, y0_dv(3), y0n_dv(3), v0(3)
    real(fp) :: lam, y_dlam, len0, r0
    real(fp) :: x0m, y0m
    real(fp), parameter :: k0min = 0.8, k0decr = 1 / 2**(1.0_fp / 8)

    x0m = sum(xy0 % x) / size(xy0)
    y0m = sum(xy0 % y) / size(xy0)

    r0 = 0.6 * maxval(sqrt((xy0 % x - x0m)**2 + (xy0 % y - y0m)**2))
    k0 = 0.25 * r0 / sqrt(real(size(xy0)))
    nmax = ceiling(log(k0min / k0) / log(k0decr))

    if (cfg_verbose) write (0, '("k0 =", g10.4)') k0
    if (cfg_verbose) write (0, '("r0 =", g10.4)') r0
    if (cfg_verbose) write (0, '("nmax =", g10.4)') nmax

    v0 = 0

    if (cfg_verbose) write (0, '(a4, a7  , a9  , 3a9  )') &
    &     'ii', 'k0', 'lam', 'dx', 'dy', 'rot'

    loop_star_sharpness: do ii = 1, nmax

      ! compute the gradient
      call comp_ydv(v0,y0,y0_dv)
      ! normalize the gradient vector length to 1
      y0n_dv = y0_dv / sqrt(sum(y0_dv**2))

      lam = -0.01

      call minimize_along_vec(lam, 1.0_fp)
      v0 = v0 + y0n_dv * lam

      if (cfg_verbose) write (0, '(i4, f7.2, f9.4, 3f9.4)') ii, k0, lam, v0

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
      y_dv(:) = 0

      xy1 % x = v(i_x) + cos(v(i_r) / r0) * (xy % x - x0m) &
      &                - sin(v(i_r) / r0) * (xy % y - y0m) + x0m
      xy1 % y = v(i_y) + sin(v(i_r) / r0) * (xy % x - x0m) &
      &                + cos(v(i_r) / r0) * (xy % y - y0m) + y0m
      xy1 % flux = xy % flux

      do i0 = 1, size(xy0)
        do i = 1, size(xy)
          aa = sqrt((xy1(i) % x - xy0(i0) % x)**2 &
          &   + (xy1(i) % y - xy0(i0) % y)**2 + k0**2 )
          bb = sqrt(xy0(i0) % flux * xy(i) % flux) * k0
          y = y + bb / aa
          y_dx1 = - (xy1(i) % x - xy0(i0) % x) * bb / aa**3
          y_dy1 = - (xy1(i) % y - xy0(i0) % y) * bb / aa**3
          x1_dr = - (sin(v(i_r) / r0) * (xy(i) % x - x0m) &
          &       +  cos(v(i_r) / r0) * (xy(i) % y - y0m)) / r0
          y1_dr =   (cos(v(i_r) / r0) * (xy(i) % x - x0m) &
          &       -  sin(v(i_r) / r0) * (xy(i) % y - y0m)) / r0
          y_dv(i_x) = y_dv(i_x) + y_dx1
          y_dv(i_y) = y_dv(i_y) + y_dy1
          y_dv(i_r) = y_dv(i_r) + y_dx1 * x1_dr + y_dy1 * y1_dr
        end do
      end do
    end subroutine

    subroutine v2mx(v,mx)
      real(fp), intent(in) :: v(3)
      real(fp), intent(out) :: mx(2,3)
      mx(1,1) =   v(i_x) + x0m * (1 - cos(v(i_r) / r0)) + y0m * sin(v(i_r) / r0)
      mx(1,2) =   cos(v(i_r) / r0)
      mx(1,3) = - sin(v(i_r) / r0)
      mx(2,1) =   v(i_y) + y0m * (1 - cos(v(i_r) / r0)) - x0m * sin(v(i_r) / r0)
      mx(2,2) =   sin(v(i_r) / r0)
      mx(2,3) =   cos(v(i_r) / r0)
    end subroutine

  end subroutine

  subroutine improject(im0, mx, im, resample)
    real(fp), dimension(:,:), intent(in), contiguous :: im0
    real(fp), intent(in) :: mx(2,3)
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

        x0 = mx(1,1) + mx(1,2) * x + mx(1,3) * y
        y0 = mx(2,1) + mx(2,2) * x + mx(2,3) * y

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
