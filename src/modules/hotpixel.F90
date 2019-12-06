module hotpixels

  use globals
  implicit none

  character(len = *), parameter :: cosme_fn = 'cosme.txt'

contains

  !----------------------------------------------------------------------------!

# ifndef _DEBUG
  pure &
# endif
  subroutine find_hot(im, hot_mask)
    use statistics, only: outliers, sigstd

    real(fp), contiguous, intent(in) :: im(:,:)
    logical, contiguous, intent(out) :: hot_mask(:,:)
    real(fp) :: av, sd, sg
    integer :: i

    hot_mask(:,:) = .true.
    call outliers(im, 3.0_fp, 5, hot_mask)
    call sigstd(im, av, sd, hot_mask)

#   ifdef _DEBUG
    write (11, '("#", a5, a8)') 'kap', 'nhot'
    do i = 0, 12
      sg = i * 1.0_fp
      hot_mask = im > av + sg * sd
      write (11, '(f6.1, i8)') sg, count(hot_mask)
    end do
#   endif

    sg = 5.0_fp
    hot_mask = im > av + sg * sd
  end subroutine

  !----------------------------------------------------------------------------!

  pure subroutine fix_hot(im, hot_mask)
    use statistics, only: quickselect
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

  subroutine write_hot(hot_mask)
    logical, contiguous :: hot_mask(:,:)
    integer :: i, j

    open (33, file = cosme_fn, action = 'write')
    write (33, *) size(hot_mask, 1), size(hot_mask, 2)

    do j = 1, size(hot_mask, 2)
      do i = 1, size(hot_mask, 1)
        if (hot_mask(i,j)) write (33, *) i, j
      end do
    end do

    close(33)
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine read_hot(hot_mask)
    logical, contiguous :: hot_mask(:,:)
    integer :: i, j, ios

    hot_mask(:,:) = .false.

    open (33, file = cosme_fn, action = 'read')
    read (33, *) i, j
    if (size(hot_mask, 1) /= i .or. size(hot_mask, 2) /= j) error stop

    do
      read (33, *, iostat = ios) i, j
      if (ios /= 0) exit
      if (i < 1 .or. i > size(hot_mask, 1) .or. j < 1 .or. j > size(hot_mask, 2)) &
      &   error stop
      hot_mask(i, j) = .true.
    end do

    close(33)

#   ifdef _DEBUG
    print '("read_hot n = ", i0)', count(hot_mask)
#   endif
  end subroutine

  !----------------------------------------------------------------------------!

end module
