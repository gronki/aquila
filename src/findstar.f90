module findstar

  use globals


  implicit none

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

  subroutine aqfindstar(im, list, nstars, threshold)
    real, dimension(:,:), intent(in) :: im
    real, intent(in), optional :: threshold
    type(starstruct), dimension(:), intent(out) :: list
    integer, intent(out) :: nstars

    logical, dimension(size(im,1), size(im,2)) :: mask, master_mask
    integer, dimension(size(im,1), size(im,2)) :: xx, yy
    ! real :: xax(size(im,1),size(im,2)), yax(size(im,1),size(im,2))
    integer :: i, ix,iy,nx,ny, xymax(2)
    real(dp) :: mt, cx, cy, mtxx, mtyy, mtxy, mtr1, mtr2, mtr3, sthr
    real :: t

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
    master_mask = (im >= sthr)

    list % x = 0
    list % y = 0
    list % v = -1
    nstars = 0

    do i = 1, size(list)
      ! if none left, exit
      if ( .not.any(master_mask) ) exit
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

      if (count(mask) < 5) cycle

      mt  = sum(im - sthr, mask)

      cx = sum((im - sthr) * xx, mask) / mt
      cy = sum((im - sthr) * yy, mask) / mt

      ! mtxx = sum(im * (xx - cx)**2, mask) / mt
      ! mtyy = sum(im * (yy - cy)**2, mask) / mt
      mtr1 = sum((im - sthr) * sqrt((xx - cx)**2 + (yy - cy)**2), mask) / mt
      mtr2 = sum((im - sthr) *     ((xx - cx)**2 + (yy - cy)**2), mask) / mt
      mtr3 = sum((im - sthr) * sqrt((xx - cx)**2 + (yy - cy)**2)**3, mask) / mt
      ! mtyy = sum(im * (yy - cy)**2, mask) / mt
      mtxy = sum((im - sthr) * (xx - cx) * (yy - cy), mask) / mt

      list(i) % v = mt
      list(i) % x = cx
      list(i) % y = cy

      nstars = nstars + 1

      if (cfg_verbose) &
      write (0,'(I5,2F8.1,I5,2ES12.4,*(F12.5))') nstars, cx, cy, count(mask),&
        & mt, mtr2, mtxy / mtr2, mtr1**2 / mtr2, mtr3**2 / mtr2**3
    end do

  end subroutine


end module findstar
