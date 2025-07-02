module findstar

  use globals
  implicit none

  type :: point_t
    real(fp) :: x, y
  end type

  type, extends(point_t) :: source_t
    real(fp) :: flux = 0
  end type

  type, extends(source_t) :: extended_source_t
    real(fp) :: rms
    real(fp) :: deviation_xy
    real(fp) :: deviation_uv
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


  subroutine aqfindstar(im, list, limit, threshold)
    real(fp), dimension(:,:), intent(in) :: im
    type(extended_source_t), intent(out), allocatable :: list(:)
    type(extended_source_t) :: star
    integer, intent(in), optional :: limit
    real(fp), intent(in), optional :: threshold
    integer, parameter :: rslice = 16, margin = 5

    logical, dimension(:,:), allocatable :: mask, master_mask
    real(fp), dimension(:,:), allocatable :: xx, yy
    integer :: i, j, ni, nj, nx, ny, imax, jmax, xymax(2)
    real(fp) :: sthr

    ni = size(im, 1)
    ny = ni
    nj = size(im, 2)
    nx = nj

    allocate(mask(ni, nj), master_mask(ni, nj))
    allocate(xx(ni, nj), yy(ni, nj))

    do concurrent (i = 1:ni, j = 1:nj)
      call ij_to_xy(real(i, fp), real(j, fp), ni, nj, 1.0_fp, xx(i, j), yy(i, j))
    end do

    ! calculate the threshold
    sthr = 0
    if (present(threshold)) sthr = threshold
    ! we consider only pixels brighter than the threshold and far away from the edge
    master_mask = (im > sthr) &
      .and. abs(xx) < 0.5 * nx - margin  &
      .and. abs(yy) < 0.5 * ny - margin 

    if (allocated(list)) deallocate(list)
    allocate(list(0))

    extract_stars: do

      if (present(limit)) then
        if (size(list) >= limit) exit extract_stars
      end if

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
      associate (ilo => max(imax - rslice, 1), &
                 ihi => min(imax + rslice, ni), &
                 jlo => max(jmax - rslice, 1), &
                 jhi => min(jmax + rslice, nj))
        associate (c_mask => mask(ilo:ihi, jlo:jhi), &
              c_master_mask => master_mask(ilo:ihi, jlo:jhi), &
              c_im => im(ilo:ihi, jlo:jhi), &
              c_xx => xx(ilo:ihi, jlo:jhi), &
              c_yy => yy(ilo:ihi, jlo:jhi))
          ! make the child mask fill the entire blob
          call fill_mask(c_mask, c_master_mask)
          ! subtract the child mask from the major one
          c_master_mask = c_master_mask .and. (.not. c_mask)
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
      end associate

      list = [list, star]

    end do extract_stars

    block
      logical :: outlier_mask(size(list))

      call cleanup_assymetric_outliers(list, outlier_mask)

      list = pack(list, outlier_mask)
    end block

  end subroutine

  subroutine cleanup_assymetric_outliers(list, mask)

      type(extended_source_t), intent(in) :: list(:)
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
