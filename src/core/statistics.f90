module statistics

  use globals
  use ieee_arithmetic, only: ieee_is_normal
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none

  private :: average_safe_1d, average_safe_2d, average_safe_3d
  private :: average_fast_1d, average_fast_2d, average_fast_3d

  interface average_safe
    module procedure :: average_safe_1d, average_safe_2d, average_safe_3d
  end interface

  interface average
    module procedure :: average_fast_1d, average_fast_2d, average_fast_3d
  end interface

  interface avsd
    module procedure :: avsd_1d_m, avsd_1d
    module procedure :: avsd_2d_m, avsd_2d
  end interface

  interface outliers
    module procedure :: outliers_2d_mask, outliers_2d
  end interface

contains

  !----------------------------------------------------------------------------!

  subroutine mexha(sg, k)
    real(fp), intent(in) :: sg
    real(fp), intent(out) :: k(:,:)
    integer :: i,j

    do concurrent (i = 1:size(k,1), j = 1:size(k,2))
      k(i,j) = mexha0(i - real(size(k,1) + 1, fp) / 2,   &
                      j - real(size(k,2) + 1, fp) / 2, sg)
    end do

  contains

    elemental function mexha0(x,y,sg) result(yf)
      real(fp), intent(in) :: x,y,sg
      real(fp) :: yf,k
      real(fp), parameter :: pi = 4 * atan(1d0)
      k = (x**2 + y**2) / (2 * sg**2)
      yf =  (1 - k)  / (pi * sg**4) * exp(-k)
    end function

  end subroutine


  !----------------------------------------------------------------------------!
  ! quickselect algorithm
  ! translated from: Numerical recipes in C

  function quickselect(arr, k) result(median)
    real(fp), intent(inout) :: arr(:)
    real(fp) :: a, median
    integer, intent(in) :: k
    integer :: i, j, lo, hi, mid

    lo = 1
    hi = size(arr)

    main_loop: do
      if (hi <= lo+1) then

        if (hi == lo+1 .and. arr(hi) < arr(lo)) call swap(arr(lo),arr(hi))
        median = arr(k)
        exit main_loop

      else

        mid = (lo + hi) / 2
        call swap(arr(mid), arr(lo+1))
        if (arr(lo  ) > arr(hi  )) call swap(arr(lo  ), arr(hi  ))
        if (arr(lo+1) > arr(hi  )) call swap(arr(lo+1), arr(hi  ))
        if (arr(lo  ) > arr(lo+1)) call swap(arr(lo  ), arr(lo+1))

        i = lo + 1
        j = hi
        a = arr(lo + 1)

        inner: do
          do
            i = i + 1
            if (arr(i) >= a) exit
          end do
          do
            j = j - 1
            if (arr(j) <= a) exit
          end do
          if (j < i) exit inner
          call swap(arr(i), arr(j))
        end do inner

        arr(lo+1) = arr(j)
        arr(j) = a

        if (j >= k) hi = j - 1
        if (j <= k) lo = i
      end if
    end do main_loop

  contains

    elemental subroutine swap(a,b)
      real(fp), intent(inout) :: a, b
      real(fp) :: c
      c = a
      a = b
      b = c
    end subroutine
  end function

  !----------------------------------------------------------------------------!

  pure subroutine outliers_2d_mask(im, msk, kap, niter, av, sd)
    use ieee_arithmetic, only: ieee_is_normal
    use iso_fortran_env, only: int64

    real(fp), intent(in) :: im(:,:), kap
    integer, intent(in) :: niter
    logical, intent(inout) :: msk(:,:)
    real(fp), intent(out) :: av, sd
    integer :: i
    integer(int64) :: nn, cn

    do i = 1, niter
      call avsd(im, msk, av, sd)
      nn = count(msk)
      msk = msk .and. (im >= av - kap * sd) &
                .and. (im <= av + kap * sd)

      cn = count(msk)
      if ( cn == nn ) exit
      if ( cn == 0 ) error stop 'all data rejected'
    end do
  end subroutine

  pure subroutine outliers_2d(im, kap, niter, av, sd)
    real(fp), intent(in) :: im(:,:), kap
    integer, intent(in) :: niter
    real(fp), intent(out) :: av, sd
    logical, allocatable :: msk(:,:)

    allocate(msk(size(im, 1), size(im, 2))); msk(:,:) = .true.
    call outliers_2d_mask(im, msk, kap, niter, av, sd)
    deallocate(msk)
  end subroutine

  !----------------------------------------------------------------------------!

  pure function average_fast_1d(x) result(m)
    real(fp), intent(in) :: x(:)
    real(fp) :: m
    m = sum(x) / size(x)
  end function

  pure function average_fast_2d(x) result(m)
    real(fp), intent(in) :: x(:,:)
    real(fp) :: m
    m = sum(x) / size(x)
  end function

  pure function average_fast_3d(x) result(m)
    real(fp), intent(in) :: x(:,:,:)
    real(fp) :: m
    m = sum(x) / size(x)
  end function

  !----------------------------------------------------------------------------!

  pure function average_safe_1d(x) result(m)
    use iso_fortran_env, only: int64
    real(fp), intent(in) :: x(:)
    real(fp) :: m
    integer :: i
    integer(int64) :: n

    m = 0; n = 0
    do i = 1, size(x)
      if (ieee_is_normal(x(i))) then
        m = m + x(i)
        n = n + 1
      end if
    end do
    m = m / n
  end function

  pure function average_safe_2d(x) result(m)
    use iso_fortran_env, only: int64
    real(fp), intent(in) :: x(:,:)
    real(fp) :: m
    integer :: i, j
    integer(int64) :: n

    m = 0; n = 0
    do j = 1, size(x, 2)
      do i = 1, size(x, 1)
        if (ieee_is_normal(x(i,j))) then
          m = m + x(i,j)
          n = n + 1
        end if
      end do
    end do
    m = m / n
  end function

  pure function average_safe_3d(x) result(m)
    use iso_fortran_env, only: int64
    real(fp), intent(in) :: x(:,:,:)
    real(fp) :: m
    integer :: i, j, k
    integer(int64) :: n

    m = 0; n = 0
    do k = 1, size(x, 3)
      do j = 1, size(x, 2)
        do i = 1, size(x, 1)
          if (ieee_is_normal(x(i,j,k))) then
            m = m + x(i,j,k)
            n = n + 1
          end if
        end do
      end do
    end do
    m = m / n
  end function

  !----------------------------------------------------------------------------!

  pure subroutine linfit(x, y, a, b)
    use iso_fortran_env, only: int64
    real(fp), dimension(:), intent(in) :: x, y
    ! logical, dimension(:), intent(in), optional :: mask
    real(fp), intent(out) :: a, b
    real(fp) :: xm, ym
    integer(int64) :: n

    if (size(x) /= size(y)) error stop

    n = size(x)
    xm = sum(x) / n
    ym = sum(y) / n

    a = sum((x - xm) * (y - ym)) / sum((x - xm)**2)
    b = sum(y - a * x) / n
  end subroutine

  !----------------------------------------------------------------------------!

  pure subroutine avsd_1d_m(x, msk, av, sd)
    real(fp), intent(in) :: x(:)
    logical, intent(in) :: msk(:)
    real(fp), intent(out) :: av, sd
    integer(int64) :: n

    n = count(msk)
    av = sum(x, msk) / n
    sd = sqrt(sum((x - av)**2, msk) / (n - 1))
  end subroutine

  pure subroutine avsd_1d(x, av, sd)
    real(fp), intent(in) :: x(:)
    real(fp), intent(out) :: av, sd
    integer(int64) :: n

    n = size(x)
    av = sum(x) / n
    sd = sqrt(sum((x - av)**2) / (n - 1))
  end subroutine

  pure subroutine avsd_2d_m(x, msk, av, sd)
    real(fp), intent(in) :: x(:,:)
    logical, intent(in) :: msk(:,:)
    real(fp), intent(out) :: av, sd
    integer(int64) :: n

    n = count(msk)
    av = sum(x, msk) / n
    sd = sqrt(sum((x - av)**2, msk) / (n - 1))
  end subroutine

  pure subroutine avsd_2d(x, av, sd)
    real(fp), intent(in) :: x(:,:)
    real(fp), intent(out) :: av, sd
    integer(int64) :: n

    n = size(x)
    av = sum(x) / n
    sd = sqrt(sum((x - av)**2) / (n - 1))
  end subroutine

  !----------------------------------------------------------------------------!

  pure function sigclip2(x, kap) result(xm)
    real(fp), intent(in) :: x(:), kap
    real(fp) :: xm, av, sd
    logical :: msk(size(x))
    integer :: i, imax

    call avsd(x, av, sd); xm = av
    msk(:) = .true.

    reject: do i = 1, size(x) - 2
      imax = maxloc(abs(x - av), 1, msk)
      msk(imax) = .false.
      call avsd(x, msk, av, sd)
      if (abs(x(imax) - av) <= kap * sd) exit reject
      xm = av
    end do reject
  end function

  !----------------------------------------------------------------------------!

end module
