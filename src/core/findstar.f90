module findstar

  use globals
  use iso_c_binding
  implicit none

  integer, parameter :: n_source_moments = 3

  type, bind(C) :: source_t
    real(fp) :: x, y
    real(fp) :: flux = 0
    real(fp) :: rms = 0
    real(fp) :: moments(n_source_moments)
  end type

  real(fp), parameter :: max_rms = 12

  private :: cleanup_assymetric_outliers, fill_mask

contains

  !----------------------------------------------------------------------------!

  pure subroutine fill_mask(mask, master_mask)
    logical(c_bool), intent(inout) :: mask(:,:)
    logical(c_bool), intent(in) :: master_mask(:,:)
    logical(c_bool), allocatable :: mask0(:,:)
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

  subroutine aqfindstar(im, master_mask, ni, nj, list, limit, rslice, margin, nstar) bind(C)
    integer(c_int64_t), intent(in), value :: ni, nj, limit
    integer(c_int64_t), intent(in), value :: rslice, margin
    real(fp), dimension(:,:), intent(in) :: im(ni,nj)
    logical(c_bool) :: master_mask(:,:)
    type(source_t), intent(out) :: list(limit)
    integer(c_int64_t), intent(out) :: nstar
    
    type(source_t) :: star
    logical(c_bool), dimension(:,:), allocatable :: mask
    real(fp), dimension(:,:), allocatable :: xx, yy
    integer :: i, j, nx, ny, imax, jmax, xymax(2)
    integer :: ilo, ihi, jlo, jhi

    ny = ni
    nx = nj
    nstar = 0

    allocate(mask(ni, nj))
    allocate(xx(ni, nj), yy(ni, nj))

    do concurrent (i = 1:ni, j = 1:nj)
      call ij_to_xy(real(i, fp), real(j, fp), ny, nx, 1.0_fp, xx(i, j), yy(i, j))
    end do

    ! calculate the threshold
    ! we consider only pixels brighter than the threshold and far away from the edge
    master_mask = master_mask &
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

          
          if (rms > max_rms) cycle extract_stars
            
            rms = sqrt(sum(c_im * ((c_xx - cx)**2 + (c_yy - cy)**2), c_mask) / flx)
            star % moments(1) = sum(c_im * (c_xx - cx) * (c_yy - cy), c_mask) / (flx * rms**2)
            star % moments(2) = sum(c_im * ((c_xx - cx)**2 - (c_yy - cy)**2) / 2, c_mask) / (flx * rms**2)
            star % moments(3) = sum(c_im * ((c_xx - cx)**2 + (c_yy - cy)**2)**2, mask=c_mask) / (flx * rms**4)

          end associate
      end associate

      nstar = nstar + 1
      list(nstar) = star

    end do extract_stars

    block
      logical(c_bool), allocatable :: outlier_mask(:)
      integer(c_size_t) :: nstar_clean, istar

      allocate(outlier_mask(nstar))
      call cleanup_assymetric_outliers(list(:nstar), outlier_mask(:nstar))

      if (cfg_verbose) then
        print '(a)', "*** STAR LIST *** "
        do i = 1, nstar
          print '(i5, 2f8.2, e10.2, 4f8.2, A3)', i, list(i)%x, list(i)%y, list(i)%flux, &
            list(i)%rms, list(i)%moments, merge(' ', 'X', outlier_mask(i))
        end do
      end if
      
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
      logical(c_bool), intent(out) :: mask(:)
      logical(c_bool), allocatable :: new_mask(:)
      integer :: n_star
      integer :: i
      character(*), parameter :: fmt1 = '("iter =", i3, 3x, "asymm = ", f5.3, 3x, "max = ", f5.3)'

      n_star = size(list)

      if (cfg_verbose) write (0, *) '-- OUTLIER CLEANUP'

      allocate(new_mask(n_star))

      mask(:) = .true.

      remove_outliers: do i = 1, n_star

        call asymmetry_test(list, mask, new_mask)
        if (all(mask .eqv. new_mask)) return

        mask(:) = new_mask(:)

      end do remove_outliers

    end subroutine cleanup_assymetric_outliers

    subroutine asymmetry_test(list, mask, mask_out)

      type(source_t) :: list(:)
      logical(c_bool) :: mask(:), mask_out(:)
      integer :: n_star, n_star_mask
      real(fp) :: asymmetry_av(n_source_moments), asymmetry_sd(n_source_moments), asymmetry_av_dev
      real(fp), allocatable :: asymmetry_dev(:)
      integer :: i

      n_star = size(list)
      n_star_mask = count(mask)

      if (.not. any(mask)) then
        mask_out(:) = .false.
        return
      end if

      allocate(asymmetry_dev(n_star), source=0._fp)

      do i = 1, n_source_moments
        asymmetry_av(i) = sum(list % moments(i), mask) / n_star_mask
        asymmetry_sd(i) = sqrt(sum((list % moments(i) - asymmetry_av(i))**2, mask) / n_star_mask)
        asymmetry_dev(:) = asymmetry_dev(:) + ((list%moments(i) - asymmetry_av(i)) / asymmetry_sd(i))**2
      end do

      asymmetry_dev(:) = sqrt(asymmetry_dev(:) / n_source_moments)

      if (cfg_verbose) then
        print '(a, i5, a, i5, a, *(f10.3,"+-",f10.3,:,", "))', "average asymmetry: ", n_star_mask, "/", n_star,&
         ":   ", (asymmetry_av(i), asymmetry_sd(i), i = 1, n_source_moments)
      end if
      
      mask_out(:) = asymmetry_dev < 2.5

    end subroutine

end module findstar
