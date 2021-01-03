module hotpixels

  use globals
  implicit none

  real(fp) :: hotpixel_threshold_sigma = 5.0_fp

contains

  !----------------------------------------------------------------------------!

# ifndef _DEBUG
  pure &
# endif
  subroutine find_hot(im, hot_mask)
    use statistics, only: outliers, avsd

    real(fp), contiguous, intent(in) :: im(:,:)
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

    sg = hotpixel_threshold_sigma
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

end module
