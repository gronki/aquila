module hotpixels

  use globals
  implicit none

contains

  !----------------------------------------------------------------------------!

# ifndef _DEBUG
  pure &
# endif
  subroutine find_hot(im, sigma_max, hot_mask)
    use statistics, only: outliers, avsd

    real(fp), contiguous, intent(in) :: im(:,:)
    real(fp), intent(in) :: sigma_max
    logical, contiguous, intent(out) :: hot_mask(:,:)
    real(fp) :: av, sd, sg
    integer :: i

    hot_mask(:,:) = .true.
    call outliers(im, hot_mask, 3.0_fp, 4, av, sd)

#   ifdef _DEBUG
    write (*, '("#", a5, a8)') 'kap', 'nhot'
    do i = 0, 12
      sg = i * 1.0_fp
      hot_mask = im > av + sg * sd
      write (*, '(f6.1, i8)') sg, count(hot_mask)
    end do
#   endif

    sg = sigma_max
    hot_mask = im > av + sg * sd
  end subroutine

  !----------------------------------------------------------------------------!

  pure subroutine fix_hot(im, hot_mask)
    ! use statistics, only: quickselect
    real(fp), contiguous, intent(inout) :: im(:,:)
    logical, contiguous, intent(in) :: hot_mask(:,:)
    integer :: i, j, n
    integer, parameter :: r = 3
    real(fp) :: a((2 * r + 1)**2)

    do j = 1, size(im, 2)
      do i = 1, size(im, 1)
        if (hot_mask(i,j)) then
          associate (ilo => max(i - r, 1), ihi => min(i + r, size(im, 1)), &
            & jlo => max(j - r, 1), jhi => min(j + r, size(im, 2)))
            associate (imc => im(ilo:ihi, jlo:jhi), msc => .not. hot_mask(ilo:ihi, jlo:jhi))
              ! trzeba zrobic pure quickselect
              ! n = count(msc)
              ! a(1:n) = pack(imc, msc)
              ! im(i, j) = quickselect(a(1:n), (n + 1) / 2)
              im(i, j) = sum(imc, msc) / count(msc)
            end associate
          end associate
        end if
      end do
    end do
  end subroutine

  !----------------------------------------------------------------------------!
#ifndef _DEBUG
  pure &
#endif
  subroutine optimize_dark_frame(light, dark, xlo0, xhi0, xm, msk)
    real(fp), intent(IN) :: light(:,:), dark(:,:)
    logical, intent(in), optional :: msk(:,:)
    real(fp), intent(in) :: xlo0, xhi0
    ! logical, intent(IN) :: is_log
    real(fp), intent(out) :: xm
    real(fp) :: xlo, xhi, ylo, yhi, ym
    real(fp) :: dark_norm(size(light,1), size(light,2))
    integer :: it

    dark_norm(:,:) = dark
    call normalize(dark_norm)

    xlo = xlo0
    xhi = xhi0
    ylo = F(xlo0)
    yhi = F(xhi0)

    do it = 1, 16

      ! if both ends have the same sign, just assume the
      ! boundary of the interval as a result
      if (ylo * yhi > 0) then
        if (abs(ylo) < abs(yhi)) then
          xm = xlo
        else
          xm = xhi
        end if
        exit
      end if

      xm = (xlo + xhi) / 2
      ym = F(xm)

#     ifdef _DEBUG
      print '(a, 3(f12.4,e11.3))', 'darkopt', xlo, ylo, xm, ym, xhi, yhi
#     endif

      if (ylo * ym <= 0) then
        xhi = xm
        yhi = ym
      end if

      if (yhi * ym <= 0) then
        xlo = xm
        ylo = ym
      end if

    end do

    contains

      pure subroutine normalize(x)
        use statistics, only: avsd
        real(fp), intent(inout) :: x(:,:)
        real(fp) :: av, sd
        if (present(msk)) then
          call avsd(x, msk, av, sd)
        else
          call avsd(x, av, sd)
        end if
        x(:,:) = (x - av) / sd
      end subroutine
    
      pure function F(x)
        real(fp), intent(in) :: x
        real(fp) :: f
        real(fp) :: light_norm(size(light,1), size(light,2))

        light_norm = light - x * dark
        call normalize(light_norm)

        f = sum(light_norm * dark_norm, msk)
      end function

  end subroutine
  
  !----------------------------------------------------------------------------!

  pure subroutine optimize_dark_frame_fast(light, dark, a, msk)
    use iso_fortran_env, only: int64
    real(fp), intent(IN) :: light(:,:), dark(:,:)
    logical, intent(in), optional :: msk(:,:)
    ! logical, intent(IN) :: is_log
    real(fp), intent(out) :: a
    integer(int64) :: n
    real(fp) :: light_av, dark_av

    n = size(light)
    if (present(msk)) n = count(msk)

    light_av = sum(light, msk) / n
    dark_av = sum(dark, msk) / n

    a = sum((light - light_av) * (dark - dark_av), msk) / sum((dark - dark_av)**2, msk)
  end subroutine

  !----------------------------------------------------------------------------!

end module
