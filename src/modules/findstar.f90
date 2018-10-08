module findstar

  use globals
  implicit none

  type :: point
    real(fp) :: x, y
  end type

  type, extends(point) :: source
    real(fp) :: flux = 0
  end type

  type, extends(source) :: extended_source
    real(fp) :: rms
    real(fp) :: deviation_xy
    real(fp) :: deviation_uv
  end type

contains

  !--------------------------------------------------------------------!

  subroutine grow_mask(mask,r)

    logical, intent(inout) :: mask(:,:)
    logical, dimension(:,:), allocatable :: mask0
    integer, intent(in) :: r
    integer :: x, y, x0, x1, y0, y1, nx, ny

    nx = size(mask,1)
    ny = size(mask,2)

    mask0 = mask

    do y = 1,ny
      do x = 1,nx
        if ( mask0(x,y) ) then
          x0 = max(x - r, 1)
          x1 = min(x + r, nx)
          y0 = max(y - r, 1)
          y1 = min(y + r, ny)
          mask(x0:x1,y0:y1) = .true.
        end if
      end do
    end do

    deallocate(mask0)

  end subroutine

  !--------------------------------------------------------------------!

  subroutine shrink_mask(mask,r)

    logical, intent(inout) :: mask(:,:)
    logical, dimension(:,:), allocatable :: mask0
    integer, intent(in) :: r
    integer :: x, y, x0, x1, y0, y1, nx, ny

    nx = size(mask,1)
    ny = size(mask,2)

    mask0 = mask

    do y = 1,ny
      do x = 1,nx
        if ( .not. mask0(x,y) ) then
          x0 = max(x - r, 1)
          x1 = min(x + r, nx)
          y0 = max(y - r, 1)
          y1 = min(y + r, ny)
          mask(x0:x1,y0:y1) = .false.
        end if
      end do
    end do

    deallocate(mask0)

  end subroutine

  !--------------------------------------------------------------------!

  subroutine fill_mask(mask,master_mask)
    logical, intent(in) :: master_mask(:,:)
    logical, intent(inout) :: mask(size(master_mask,1),size(master_mask,2))
    logical :: mask0(size(master_mask,1),size(master_mask,2))

    do
      mask0 = mask
      call grow_mask(mask,1)
      mask = mask .and. master_mask
      if ( all(mask0.eqv.mask) ) exit
    end do
  end subroutine

  subroutine aqfindstar(im, list, limit, threshold)
    real(fp), dimension(:,:), intent(in) :: im
    type(extended_source), intent(out), allocatable :: list(:)
    type(extended_source) :: star
    integer, intent(in), optional :: limit
    real(fp), intent(in), optional :: threshold

    logical, dimension(size(im,1), size(im,2)) :: mask, master_mask
    integer, dimension(size(im,1), size(im,2)) :: xx, yy
    ! real(fp) :: xax(size(im,1),size(im,2)), yax(size(im,1),size(im,2))
    integer :: i, ix,iy,nx,ny, xymax(2)
    real(fp) :: sthr

    nx = size(im,1)
    ny = size(im,2)

    forall (ix = 1:nx, iy = 1:ny)
      xx(ix,iy) = ix
      yy(ix,iy) = iy
    end forall

    ! calculate the threshold
    sthr = 0
    if (present(threshold)) sthr = threshold
    ! we consider only pixels brighter than the threshold
    master_mask = (im > sthr)

    if (allocated(list)) deallocate(list)
    allocate(list(0))

    extract_stars: do

      if (present(limit)) then
        if (size(list) >= limit) exit extract_stars
      end if

      ! if none left, exit
      if ( .not. any(master_mask) ) exit extract_stars

      ! localize maximum pixel within good pixels
      xymax = maxloc(im, master_mask)
      ! set this pixel to true in child mask
      mask = .false.
      mask(xymax(1),xymax(2)) = .true.
      ! make the child mask fill the entire blob
      call fill_mask(mask,master_mask)
      ! subtract the child mask from the major one
      master_mask = master_mask .and. (.not. mask)
      ! extend the child mask by one
      ! call grow_mask(mask,1)

      if (count(mask) < 8) cycle

      associate (flx => star % flux, cx => star % x, cy => star % y, rms => star % rms)

        flx = sum(im, mask)
        cx = sum(im * xx, mask) / flx
        cy = sum(im * yy, mask) / flx

        rms = sqrt(sum(im * ((xx - cx)**2 + (yy - cy)**2), mask) / flx)

        if (rms > 10) cycle extract_stars

        star % deviation_xy = sum(im * (xx - cx) * (yy - cy), mask) / (flx * rms**2)
        star % deviation_uv = sum(im * ((xx - cx)**2 - (yy - cy)**2) / 2, mask) / (flx * rms**2)

      end associate

      list = [list, star]

    end do extract_stars

    cleanup_assymetric_outliers: block

      logical :: mask(size(list))
      real(fp) :: asymmetry(size(list))
      integer :: ix_max
      real(fp) :: deviation
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

      list = pack(list, mask)

    end block cleanup_assymetric_outliers

  end subroutine

end module findstar
