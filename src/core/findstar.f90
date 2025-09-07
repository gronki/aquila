module findstar

  use globals
  use iso_c_binding
  implicit none

  type, bind(C) :: source_t
    real(fp) :: x, y
    real(fp) :: flux = 0
    real(fp) :: rms = 0
    real(fp) :: deviation_xy = 0
    real(fp) :: deviation_uv = 0
  end type

  real(fp), parameter :: max_rms = 12

  private :: cleanup_assymetric_outliers, fill_mask

contains

  !----------------------------------------------------------------------------!

  pure subroutine fill_mask(mask, master_mask)
    logical, intent(inout) :: mask(:,:)
    logical, intent(in) :: master_mask(:,:)
    logical, allocatable :: mask0(:,:)
    integer :: i, j

    ! important: mask must be zero by the edge!

    growmask: do
      mask0 = mask
      do concurrent (i = 2:size(mask, 1) - 1, j = 2:size(mask, 2) - 1)
        mask(i,j) = mask0(i,j) .or. mask0(i-1,j) .or. mask0(i+1,j) .or. mask0(i,j-1) .or. mask0(i,j+1)
      end do
      mask = mask .and. master_mask
      if (all(mask0 .eqv. mask)) exit growmask
    end do growmask
  end subroutine

  !----------------------------------------------------------------------------!

  elemental subroutine ij_to_xy(i, j, ni, nj, scale, x, y)
    real(fp), intent(in) :: i, j, scale
    integer, intent(in) :: ni, nj
    real(fp), intent(out) :: x, y

    x =   (j - 0.5_fp * (nj + 1)) / scale
    y = - (i - 0.5_fp * (ni + 1)) / scale

  end subroutine

  !----------------------------------------------------------------------------!

  elemental subroutine xy_to_ij(x, y, ni, nj, scale, i, j)
    real(fp), intent(in) :: x, y, scale
    integer, intent(in) :: ni, nj
    real(fp), intent(out) :: i, j

    j =   x * scale + 0.5_fp * (nj + 1)
    i = - y * scale + 0.5_fp * (ni + 1)

  end subroutine

  !----------------------------------------------------------------------------!


  subroutine aqfindstar(im, list, limit)
    real(fp), dimension(:,:), intent(in) :: im
    type(source_t), intent(out), allocatable :: list(:)
    type(source_t) :: star
    integer, intent(in) :: limit
    integer(c_size_t) :: nstar
    integer(c_int64_t), parameter :: rslice = 16, margin = 5

    allocate(list(limit))

    call aqfindstar_f(im, size(im, 1, c_size_t), size(im, 2, c_size_t), &
      list, int(limit, c_size_t), rslice, margin, nstar)

    list = list(:nstar)
  end subroutine

  subroutine aqfindstar_f(im, ni, nj, list, limit, rslice, margin, nstar) bind(C)
    integer(c_size_t), intent(in), value :: ni, nj, limit
    integer(c_int64_t), intent(in), value :: rslice, margin
    real(fp), dimension(:,:), intent(in) :: im(ni,nj)
    type(source_t), intent(out) :: list(limit)
    integer(c_size_t), intent(out) :: nstar
    
    type(source_t) :: star
    logical, dimension(:,:), allocatable :: mask, master_mask
    real(fp), dimension(:,:), allocatable :: xx, yy
    integer :: i, j, nx, ny, imax, jmax, xymax(2)
    integer :: ilo, ihi, jlo, jhi
    real(fp) :: sthr

    ny = ni
    nx = nj
    nstar = 0

    allocate(mask(ni, nj), master_mask(ni, nj))
    allocate(xx(ni, nj), yy(ni, nj))

    do concurrent (i = 1:ni, j = 1:nj)
      call ij_to_xy(real(i, fp), real(j, fp), ny, nx, 1.0_fp, xx(i, j), yy(i, j))
    end do

    ! calculate the threshold
    sthr = 0
    ! we consider only pixels brighter than the threshold and far away from the edge
    master_mask = (im > sthr) &
      .and. abs(xx) < 0.5 * nx - margin  &
      .and. abs(yy) < 0.5 * ny - margin 

    extract_stars: do

      if (nstar >= limit) exit extract_stars

      ! if none left, exit
      if (.not. any(master_mask)) exit extract_stars

      ! localize maximum pixel within good pixels
      xymax = maxloc(im, master_mask)
      imax = xymax(1)
      jmax = xymax(2)

      ! set this pixel to true in child mask
      mask(:,:) = .false.
      mask(imax, jmax) = .true.

      ! crop the image to save processing power
      ilo = max(imax - rslice, 1)
      ihi = min(imax + rslice, ni)
      jlo = max(jmax - rslice, 1)
      jhi = min(jmax + rslice, nj)
      
      associate (c_mask => mask(ilo:ihi, jlo:jhi), &
            c_master_mask => master_mask(ilo:ihi, jlo:jhi), &
            c_im => im(ilo:ihi, jlo:jhi), &
            c_xx => xx(ilo:ihi, jlo:jhi), &
            c_yy => yy(ilo:ihi, jlo:jhi))
        ! make the child mask fill the entire blob
        call fill_mask(c_mask, c_master_mask)
        ! subtract the child mask from the major one
        c_master_mask(:,:) = c_master_mask .and. (.not. c_mask)
        ! skip anything which is less than 3x3
        if (count(c_mask) < 8) cycle

        associate (flx => star % flux, cx => star % x, cy => star % y, rms => star % rms)
          flx = sum(c_im, c_mask)
          cx = sum(c_im * c_xx, c_mask) / flx
          cy = sum(c_im * c_yy, c_mask) / flx

          rms = sqrt(sum(c_im * ((c_xx - cx)**2 + (c_yy - cy)**2), c_mask) / flx)

          if (rms > max_rms) cycle extract_stars

          star % deviation_xy = sum(c_im * (c_xx - cx) * (c_yy - cy), c_mask) / (flx * rms**2)
          star % deviation_uv = sum(c_im * ((c_xx - cx)**2 - (c_yy - cy)**2) / 2, c_mask) / (flx * rms**2)
        end associate
      end associate

      nstar = nstar + 1
      list(nstar) = star

    end do extract_stars

    block
      logical, allocatable :: outlier_mask(:)
      integer(c_size_t) :: nstar_clean, istar

      allocate(outlier_mask(nstar))
      call cleanup_assymetric_outliers(list(:nstar), outlier_mask(:nstar))
      
      nstar_clean = 0
      do istar = 1, nstar
        if (.not. outlier_mask(istar)) cycle
        nstar_clean = nstar_clean + 1
        if (nstar_clean == istar) cycle
        list(nstar_clean) = list(istar)
      end do
      nstar = nstar_clean

    end block

  end subroutine

  subroutine cleanup_assymetric_outliers(list, mask)

      type(source_t), intent(in) :: list(:)
      logical, intent(out) :: mask(:)
      real(fp) :: asymmetry(size(list))
      integer :: ix_max
      real(fp) :: deviation
      integer :: i
      character(*), parameter :: fmt1 = '("iter =", i3, 3x, "asymm = ", f5.3, 3x, "max = ", f5.3)'

      if (cfg_verbose) write (0, *) '-- OUTLIER CLEANUP'

      mask(:) = .true.
      asymmetry(:) = sqrt(list % deviation_xy**2 + list % deviation_uv**2)

      remove_outliers: do i = 1, size(list)

        ! localize the largest element in the array and remove it
        ix_max = maxloc(asymmetry, 1, mask)
        mask(ix_max) = .false.

        ! compute the deviation (ideally, asymmetry = 0)
        deviation = sqrt(sum(asymmetry**2, mask) / (count(mask) - 1))

        if (cfg_verbose) write (0, fmt1) i, asymmetry(ix_max), 3 * deviation

        ! if not an outlier, set back to true and exit the loop
        if (asymmetry(ix_max) <= 3 * deviation) then
          if (cfg_verbose) write (0, *) '-- outlier cleanup finished'
          mask(ix_max) = .true.
          exit remove_outliers
        end if

      end do remove_outliers

    end subroutine cleanup_assymetric_outliers

end module findstar
