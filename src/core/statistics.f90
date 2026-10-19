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
module procedure :: avsd_r32_1d_m, avsd_r32_1d
module procedure :: avsd_r32_2d_m, avsd_r32_2d
module procedure :: avsd_r64_1d_m, avsd_r64_1d
module procedure :: avsd_r64_2d_m, avsd_r64_2d
end interface

interface outliers
module procedure :: outliers_2d_mask, outliers_2d
end interface

interface outliers_fixed
module procedure :: outliers_fixed_2d_mask, outliers_fixed_2d
end interface

interface linfit
module procedure :: linfit_r32
module procedure :: linfit_r64
end interface

interface linfit2d
module procedure :: linfit2d_r32
module procedure :: linfit2d_r64
end interface

interface sigclip2
module procedure :: sigclip2_r32
module procedure :: sigclip2_r64
end interface

interface sigclip3
module procedure :: sigclip3_r32
module procedure :: sigclip3_r64
end interface

interface sigclip2_fixed
module procedure :: sigclip2_fixed_r32
module procedure :: sigclip2_fixed_r64
end interface

interface quickselect
module procedure :: quickselect_r32
module procedure :: quickselect_r64
end interface

integer, parameter :: clip_both = 0, clip_hi = 1, clip_lo = 2

contains

 !----------------------------------------------------------------------------!

subroutine mexha(sg, k)
   real(r32_k), intent(in) :: sg
   real(r32_k), intent(out) :: k(:,:)
   integer :: i,j

   do concurrent (i = 1:size(k,1), j = 1:size(k,2))
      k(i,j) = mexha0(i - real(size(k,1) + 1, r32_k) / 2,   &
         j - real(size(k,2) + 1, r32_k) / 2, sg)
   end do

contains

   elemental function mexha0(x,y,sg) result(yf)
      real(r32_k), intent(in) :: x,y,sg
      real(r32_k) :: yf,k
      real(r32_k), parameter :: pi = 4 * atan(1e0)
      k = (x**2 + y**2) / (2 * sg**2)
      yf =  (1 - k)  / (pi * sg**4) * exp(-k)
   end function

end subroutine


 !----------------------------------------------------------------------------!
 ! quickselect algorithm
 ! translated from: Numerical recipes in C

function quickselect_r32(arr, k) result(median)
   real(r32_k), intent(inout) :: arr(:)
   real(r32_k) :: a, median
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
      real(r32_k), intent(inout) :: a, b
      real(r32_k) :: c
      c = a
      a = b
      b = c
   end subroutine
end function

function quickselect_r64(arr, k) result(median)
   real(r64_k), intent(inout) :: arr(:)
   real(r64_k) :: a, median
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
      real(r64_k), intent(inout) :: a, b
      real(r64_k) :: c
      c = a
      a = b
      b = c
   end subroutine
end function

 !----------------------------------------------------------------------------!

pure subroutine outliers_2d_mask(im, msk, kap, niter, av, sd)
   use ieee_arithmetic, only: ieee_is_normal
   use iso_fortran_env, only: int64

   real(r32_k), intent(in) :: im(:,:), kap
   integer, intent(in) :: niter
   logical, intent(inout) :: msk(:,:)
   real(r32_k), intent(out) :: av, sd
   integer :: i
   integer(int64) :: nn, cn

   do i = 1, niter
      call avsd(im, msk, av, sd)
      nn = count(msk, kind=int64)
      msk = msk .and. (im >= av - kap * sd) &
         .and. (im <= av + kap * sd)

      cn = count(msk, kind=int64)
      if ( cn == nn ) exit
      if ( cn == 0 ) error stop 'all data rejected'
   end do
end subroutine

pure subroutine outliers_2d(im, kap, niter, av, sd)
   real(r32_k), intent(in) :: im(:,:), kap
   integer, intent(in) :: niter
   real(r32_k), intent(out) :: av, sd
   logical, allocatable :: msk(:,:)

   allocate(msk(size(im, 1), size(im, 2))); msk(:,:) = .true.
   call outliers_2d_mask(im, msk, kap, niter, av, sd)
   deallocate(msk)
end subroutine

 !----------------------------------------------------------------------------!

! Same clipping as outliers, except that the expected value is given instead
! of being measured: only the dispersion around it is re-estimated.

pure subroutine outliers_fixed_2d_mask(im, msk, kap, niter, av, sd, error_status)
   use iso_fortran_env, only: int64
   use aquila_c_binding, only: error_status_t, reset_err, pure_forbid_err, &
      failed_err, status_error

   real(r32_k), intent(in) :: im(:,:), kap, av
   integer, intent(in) :: niter
   logical, intent(inout) :: msk(:,:)
   real(r32_k), intent(out) :: sd
   type(error_status_t), intent(inout), optional :: error_status
   integer :: i
   integer(int64) :: nn, cn

   ! the loop may converge and exit before any check runs
   if (present(error_status)) call reset_err(error_status)

   do i = 1, niter
      nn = count(msk, kind=int64)
      ! av was not estimated from the data, so no degree of freedom is lost
      sd = sqrt(sum((im - av)**2, msk) / real(nn, r32_k))
      msk = msk .and. (im >= av - kap * sd) &
         .and. (im <= av + kap * sd)

      cn = count(msk, kind=int64)
      if ( cn == nn ) exit
      call pure_forbid_err(cn == 0, error_status, status_error, 'all data rejected')
      if (failed_err(error_status)) return
   end do
end subroutine

pure subroutine outliers_fixed_2d(im, kap, niter, av, sd, error_status)
   use aquila_c_binding, only: error_status_t
   real(r32_k), intent(in) :: im(:,:), kap, av
   integer, intent(in) :: niter
   real(r32_k), intent(out) :: sd
   type(error_status_t), intent(inout), optional :: error_status
   logical, allocatable :: msk(:,:)

   allocate(msk(size(im, 1), size(im, 2))); msk(:,:) = .true.
   call outliers_fixed_2d_mask(im, msk, kap, niter, av, sd, error_status)
   deallocate(msk)
end subroutine

 !----------------------------------------------------------------------------!

pure function average_fast_1d(x) result(m)
   real(r32_k), intent(in) :: x(:)
   real(r32_k) :: m
   m = sum(x) / real(size(x, kind=int64), kind=r32_k)
end function

pure function average_fast_2d(x) result(m)
   real(r32_k), intent(in) :: x(:,:)
   real(r32_k) :: m
   m = sum(x) / real(size(x, kind=int64), kind=r32_k)
end function

pure function average_fast_3d(x) result(m)
   real(r32_k), intent(in) :: x(:,:,:)
   real(r32_k) :: m
   m = sum(x) / real(size(x, kind=int64), kind=r32_k)
end function

 !----------------------------------------------------------------------------!

pure function average_safe_1d(x) result(m)
   use iso_fortran_env, only: int64
   real(r32_k), intent(in) :: x(:)
   real(r32_k) :: m
   integer :: i
   integer(int64) :: n

   m = 0; n = 0
   do i = 1, size(x)
      if (ieee_is_normal(x(i))) then
         m = m + x(i)
         n = n + 1
      end if
   end do
   m = m / real(n, r32_k)
end function

pure function average_safe_2d(x) result(m)
   use iso_fortran_env, only: int64
   real(r32_k), intent(in) :: x(:,:)
   real(r32_k) :: m
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
   m = m / real(n, r32_k)
end function

pure function average_safe_3d(x) result(m)
   use iso_fortran_env, only: int64
   real(r32_k), intent(in) :: x(:,:,:)
   real(r32_k) :: m
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
   m = m / real(n, r32_k)
end function

 !----------------------------------------------------------------------------!

pure subroutine linfit_r32(x, y, a, b)
   use iso_fortran_env, only: int64
   real(r32_k), dimension(:), intent(in) :: x, y
   ! logical, dimension(:), intent(in), optional :: mask
   real(r32_k), intent(out) :: a, b
   real(r32_k) :: xm, ym
   integer(int64) :: n

   if (size(x) /= size(y)) error stop

   n = size(x, kind=int64)
   xm = sum(x) / real(n, r32_k)
   ym = sum(y) / real(n, r32_k)

   a = sum((x - xm) * (y - ym)) / sum((x - xm)**2)
   b = sum(y - a * x) / real(n, r32_k)
end subroutine

pure subroutine linfit_r64(x, y, a, b)
   use iso_fortran_env, only: int64
   real(r64_k), dimension(:), intent(in) :: x, y
   ! logical, dimension(:), intent(in), optional :: mask
   real(r64_k), intent(out) :: a, b
   real(r64_k) :: xm, ym
   integer(int64) :: n

   if (size(x) /= size(y)) error stop

   n = size(x, kind=int64)
   xm = sum(x) / n
   ym = sum(y) / n

   a = sum((x - xm) * (y - ym)) / sum((x - xm)**2)
   b = sum(y - a * x) / n
end subroutine

 !----------------------------------------------------------------------------!

! Least squares plane z = a * x + b * y + c through scattered points.

pure subroutine linfit2d_r32(x, y, z, a, b, c, error_status)
   use iso_fortran_env, only: int64
   use aquila_c_binding, only: error_status_t, pure_forbid_err, failed_err, &
      status_error, status_err_shape_mismatch
   real(r32_k), dimension(:), intent(in) :: x, y, z
   real(r32_k), intent(out) :: a, b, c
   type(error_status_t), intent(inout), optional :: error_status
   real(r32_k) :: xm, ym, zm, sxx, syy, sxy, sxz, syz, det
   integer(int64) :: n

   call pure_forbid_err(size(x) /= size(y) .or. size(x) /= size(z), error_status, &
      status_err_shape_mismatch, 'linfit2d: x, y and z differ in size')
   if (failed_err(error_status)) return

   n = size(x, kind=int64)
   xm = sum(x) / real(n, r32_k)
   ym = sum(y) / real(n, r32_k)
   zm = sum(z) / real(n, r32_k)

   sxx = sum((x - xm)**2)
   syy = sum((y - ym)**2)
   sxy = sum((x - xm) * (y - ym))
   sxz = sum((x - xm) * (z - zm))
   syz = sum((y - ym) * (z - zm))

   det = sxx * syy - sxy**2

   ! no plane is determined if every sample sits on one line
   call pure_forbid_err(det == 0, error_status, status_error, &
      'linfit2d: samples are collinear')
   if (failed_err(error_status)) return

   a = (sxz * syy - syz * sxy) / det
   b = (syz * sxx - sxz * sxy) / det
   c = zm - a * xm - b * ym
end subroutine

pure subroutine linfit2d_r64(x, y, z, a, b, c, error_status)
   use iso_fortran_env, only: int64
   use aquila_c_binding, only: error_status_t, pure_forbid_err, failed_err, &
      status_error, status_err_shape_mismatch
   real(r64_k), dimension(:), intent(in) :: x, y, z
   real(r64_k), intent(out) :: a, b, c
   type(error_status_t), intent(inout), optional :: error_status
   real(r64_k) :: xm, ym, zm, sxx, syy, sxy, sxz, syz, det
   integer(int64) :: n

   call pure_forbid_err(size(x) /= size(y) .or. size(x) /= size(z), error_status, &
      status_err_shape_mismatch, 'linfit2d: x, y and z differ in size')
   if (failed_err(error_status)) return

   n = size(x, kind=int64)
   xm = sum(x) / n
   ym = sum(y) / n
   zm = sum(z) / n

   sxx = sum((x - xm)**2)
   syy = sum((y - ym)**2)
   sxy = sum((x - xm) * (y - ym))
   sxz = sum((x - xm) * (z - zm))
   syz = sum((y - ym) * (z - zm))

   det = sxx * syy - sxy**2

   ! no plane is determined if every sample sits on one line
   call pure_forbid_err(det == 0, error_status, status_error, &
      'linfit2d: samples are collinear')
   if (failed_err(error_status)) return

   a = (sxz * syy - syz * sxy) / det
   b = (syz * sxx - sxz * sxy) / det
   c = zm - a * xm - b * ym
end subroutine

 !----------------------------------------------------------------------------!

pure subroutine avsd_r32_1d_m(x, msk, av, sd)
   real(r32_k), intent(in) :: x(:)
   logical, intent(in) :: msk(:)
   real(r32_k), intent(out) :: av, sd
   integer(int64) :: n

   n = count(msk, kind=int64)
   av = sum(x, msk) / real(n, r32_k)
   sd = sqrt(sum((x - av)**2, msk) / real(n - 1, r32_k))
end subroutine

pure subroutine avsd_r32_1d(x, av, sd)
   real(r32_k), intent(in) :: x(:)
   real(r32_k), intent(out) :: av, sd
   integer(int64) :: n

   n = size(x, kind=int64)
   av = sum(x) / real(n, r32_k)
   sd = sqrt(sum((x - av)**2) / real(n - 1, r32_k))
end subroutine


pure subroutine avsd_r32_2d_m(x, msk, av, sd)
   real(r32_k), intent(in) :: x(:,:)
   logical, intent(in) :: msk(:,:)
   real(r32_k), intent(out) :: av, sd
   integer(int64) :: n

   n = count(msk, kind=int64)
   av = sum(x, msk) / real(n, r32_k)
   sd = sqrt(sum((x - av)**2, msk) / real(n - 1, r32_k))
end subroutine

pure subroutine avsd_r32_2d(x, av, sd)
   real(r32_k), intent(in) :: x(:,:)
   real(r32_k), intent(out) :: av, sd
   integer(int64) :: n

   n = size(x, kind=int64)
   av = sum(x) / real(n, r32_k)
   sd = sqrt(sum((x - av)**2) / real(n - 1, r32_k))
end subroutine

subroutine avsd_r32_2d_c(x, av, sd) bind(C, name="avsd_2d")
   use aquila_c_binding
   type(buffer_descriptor_r32_t), value :: x
   real(r32_k) :: av, sd

   call avsd_r32_2d(from_descriptor(x), av, sd)
end subroutine

pure subroutine avsd_r64_1d_m(x, msk, av, sd)
   real(r64_k), intent(in) :: x(:)
   logical, intent(in) :: msk(:)
   real(r64_k), intent(out) :: av, sd
   integer(int64) :: n

   n = count(msk, kind=int64)
   av = sum(x, msk) / n
   sd = sqrt(sum((x - av)**2, msk) / (n - 1))
end subroutine

pure subroutine avsd_r64_1d(x, av, sd)
   real(r64_k), intent(in) :: x(:)
   real(r64_k), intent(out) :: av, sd
   integer(int64) :: n

   n = size(x, kind=int64)
   av = sum(x) / n
   sd = sqrt(sum((x - av)**2) / (n - 1))
end subroutine

pure subroutine avsd_r64_2d_m(x, msk, av, sd)
   real(r64_k), intent(in) :: x(:,:)
   logical, intent(in) :: msk(:,:)
   real(r64_k), intent(out) :: av, sd
   integer(int64) :: n

   n = count(msk, kind=int64)
   av = sum(x, msk) / n
   sd = sqrt(sum((x - av)**2, msk) / (n - 1))
end subroutine

pure subroutine avsd_r64_2d(x, av, sd)
   real(r64_k), intent(in) :: x(:,:)
   real(r64_k), intent(out) :: av, sd
   integer(int64) :: n

   n = size(x, kind=int64)
   av = sum(x) / n
   sd = sqrt(sum((x - av)**2) / (n - 1))
end subroutine

 !----------------------------------------------------------------------------!

pure function sigclip2_r32(x, kap) result(av)
   real(r32_k), intent(in) :: x(:), kap
   real(r32_k) :: av, sd
   logical :: msk(size(x))
   integer :: i, imax

   call avsd(x, av, sd)
   msk(:) = .true.

   reject: do i = 1, size(x) - 2
      imax = maxloc(abs(x - av), 1, msk)
      msk(imax) = .false.
      call avsd(x, msk, av, sd)
      if (abs(x(imax) - av) <= kap * sd) then
         msk(imax) = .true.
         exit reject
      end if
   end do reject
   call avsd(x, msk, av, sd)
end function

pure function sigclip2_r64(x, kap) result(av)
   real(r64_k), intent(in) :: x(:), kap
   real(r64_k) :: av, sd
   logical :: msk(size(x))
   integer :: i, imax

   call avsd(x, av, sd)
   msk(:) = .true.

   reject: do i = 1, size(x) - 2
      imax = maxloc(abs(x - av), 1, msk)
      msk(imax) = .false.
      call avsd(x, msk, av, sd)
      if (abs(x(imax) - av) <= kap * sd) then
         msk(imax) = .true.
         exit reject
      end if
   end do reject
   call avsd(x, msk, av, sd)
end function

 !----------------------------------------------------------------------------!

! Same rejection as sigclip2, except that the expected value is given instead
! of being measured, so what comes back is the dispersion around it.

pure function sigclip2_fixed_r32(x, kap, av) result(sd)
   real(r32_k), intent(in) :: x(:), kap, av
   real(r32_k) :: sd
   logical :: msk(size(x))
   integer :: i, imax

   msk(:) = .true.
   ! av was not estimated from the data, so no degree of freedom is lost
   sd = sqrt(sum((x - av)**2, msk) / real(count(msk), r32_k))

   reject: do i = 1, size(x) - 2
      imax = maxloc(abs(x - av), 1, msk)
      msk(imax) = .false.
      sd = sqrt(sum((x - av)**2, msk) / real(count(msk), r32_k))
      if (abs(x(imax) - av) <= kap * sd) then
         msk(imax) = .true.
         exit reject
      end if
   end do reject
   sd = sqrt(sum((x - av)**2, msk) / real(count(msk), r32_k))
end function

 !----------------------------------------------------------------------------!

! Same rejection as sigclip2, except that the expected value is given instead
! of being measured, so what comes back is the dispersion around it.

pure function sigclip2_fixed_r64(x, kap, av) result(sd)
   real(r64_k), intent(in) :: x(:), kap, av
   real(r64_k) :: sd
   logical :: msk(size(x))
   integer :: i, imax

   msk(:) = .true.
   ! av was not estimated from the data, so no degree of freedom is lost
   sd = sqrt(sum((x - av)**2, msk) / real(count(msk), r64_k))

   reject: do i = 1, size(x) - 2
      imax = maxloc(abs(x - av), 1, msk)
      msk(imax) = .false.
      sd = sqrt(sum((x - av)**2, msk) / real(count(msk), r64_k))
      if (abs(x(imax) - av) <= kap * sd) then
         msk(imax) = .true.
         exit reject
      end if
   end do reject
   sd = sqrt(sum((x - av)**2, msk) / real(count(msk), r64_k))
end function

 !----------------------------------------------------------------------------!



pure subroutine sigclip3_r32(x, msk, kap, av, sd, mode, fix_av)
   real(r32_k), intent(in) :: x(:), kap
   real(r32_k), intent(out) :: av, sd
   real(r32_k), intent(in), optional :: fix_av
   integer, intent(in), optional :: mode
   integer :: mode_
   logical, intent(inout) :: msk(:)
   integer(int64) :: i, imax, mask_count

   mode_ = clip_both
   if (present(mode)) mode_ = mode

   mask_count = count(msk, kind=int64)
   if (mode_ == clip_both) av = sum(x, msk) / mask_count

   reject: do i = 1, size(x, kind=int64) - 2

      if (mask_count < 3) exit

      select case (mode_)
      case (clip_both)
         imax = maxloc(abs(x - av), 1, msk, kind=int64)
      case (clip_hi)
         imax = maxloc(x, 1, msk, kind=int64)
      case (clip_lo)
         imax = minloc(x, 1, msk, kind=int64)
      case default
         error stop "incorrect mode value in sigclip3!"
      end select

      msk(imax) = .false.
      mask_count = count(msk, kind=int64)

      if (present(fix_av)) then
         av = fix_av
      else
         av = sum(x, msk) / mask_count
      end if
      sd = sqrt(sum( (x - av)**2, msk ) / (mask_count - 1))

      if (abs(x(imax) - av) <= kap * sd) then
         msk(imax) = .true.
         exit reject
      end if

   end do reject

   call avsd(x, msk, av, sd)

end subroutine

pure subroutine sigclip3_r64(x, msk, kap, av, sd, mode, fix_av)
   real(r64_k), intent(in) :: x(:), kap
   real(r64_k), intent(out) :: av, sd
   real(r64_k), intent(in), optional :: fix_av
   integer, intent(in), optional :: mode
   integer :: mode_
   logical, intent(inout) :: msk(:)
   integer(int64) :: i, imax, mask_count

   mode_ = clip_both
   if (present(mode)) mode_ = mode

   mask_count = count(msk, kind=int64)
   if (mode_ == clip_both) av = sum(x, msk) / mask_count

   reject: do i = 1, size(x, kind=int64) - 2

      if (mask_count < 3) exit

      select case (mode_)
      case (clip_both)
         imax = maxloc(abs(x - av), 1, msk, kind=int64)
      case (clip_hi)
         imax = maxloc(x, 1, msk, kind=int64)
      case (clip_lo)
         imax = minloc(x, 1, msk, kind=int64)
      case default
         error stop "incorrect mode value in sigclip3!"
      end select

      msk(imax) = .false.
      mask_count = count(msk, kind=int64)

      if (present(fix_av)) then
         av = fix_av
      else
         av = sum(x, msk) / mask_count
      end if
      sd = sqrt(sum( (x - av)**2, msk ) / (mask_count - 1))

      if (abs(x(imax) - av) <= kap * sd) then
         msk(imax) = .true.
         exit reject
      end if

   end do reject

   call avsd(x, msk, av, sd)

end subroutine

end module
