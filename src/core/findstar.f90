module findstar

  use globals
  use iso_c_binding
  use hotpixels, only: fix_hot_median
  use stacking, only: mask_margins, mask_margins_c
  use source_m
  implicit none

  integer(c_int), parameter :: findstar_rejection_absolute = 1, findstar_rejection_relative = 2

  type, bind(C) :: findstar_param_t
    integer(c_int64_t) :: rslice = 16
    integer(c_int64_t) :: margin = 32
    integer(c_int64_t) :: min_star_pixels = 8
    real(c_double) :: blur_radius = 2.3
    real(c_double) :: thresh_sd = 2.
    integer(c_int) :: rejection = findstar_rejection_absolute
    real(c_double) :: max_rms = 12.
  end type

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

  subroutine aqfindstar(im, master_mask, ni, nj, list, limit, param, nstar) bind(C)
    integer(i64_k), intent(in), value :: ni, nj, limit
    type(findstar_param_t) :: param
    real(buf_k), dimension(:,:), intent(in) :: im(ni,nj)
    logical(c_bool), intent(inout) :: master_mask(:,:)
    type(source_t), intent(out) :: list(limit)
    integer(i64_k), intent(out) :: nstar
    
    type(source_t) :: star
    logical(c_bool), dimension(:,:), allocatable :: mask
    real(r64_k), dimension(:,:), allocatable :: xx, yy
    integer(i64_k) :: i, j, imax, jmax, xymax(2)
    integer(i64_k) :: ilo, ihi, jlo, jhi

    nstar = 0

    allocate(mask(ni, nj))
    allocate(xx(ni, nj), yy(ni, nj))

    do concurrent (i = 1:ni, j = 1:nj)
      call ij_to_xy(real(i, r64_k), real(j, r64_k), int(ni, i64_k), int(nj, i64_k), 1.0_r64_k, xx(i, j), yy(i, j))
    end do

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
      ilo = max(imax - param%rslice, 1)
      ihi = min(imax + param%rslice, ni)
      jlo = max(jmax - param%rslice, 1)
      jhi = min(jmax + param%rslice, nj)
      
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
          if (rms > param%max_rms) cycle extract_stars
            
            star % asymmetry_xy = sum(c_im * (c_xx - cx) * (c_yy - cy), c_mask) / (flx * rms**2)
            star % asymmetry_uv = sum(c_im * ((c_xx - cx)**2 - (c_yy - cy)**2) / 2, c_mask) / (flx * rms**2)
            star % asymmetry = sqrt(star%asymmetry_xy**2 + star%asymmetry_uv**2)
            star % kurtosis = sum(c_im * ((c_xx - cx)**2 + (c_yy - cy)**2)**2, mask=c_mask) / (flx * rms**4)
          end associate
      end associate

      call xy_to_ij(star % x, star % y, int(ni, i64_k), int(nj, i64_k), 1.0_r64_k, star % iy, star % ix)

      nstar = nstar + 1
      list(nstar) = star

    end do extract_stars

    block
      logical(c_bool), allocatable :: outlier_mask(:)
      integer(c_size_t) :: nstar_clean, istar

      allocate(outlier_mask(nstar))

      select case (param%rejection)
      case (findstar_rejection_absolute)
        call cleanup_assymetric_outliers_absolute(list(:nstar), outlier_mask(:nstar))
      case (findstar_rejection_relative)
        call cleanup_assymetric_outliers_relative(list(:nstar), outlier_mask(:nstar))
      case default
        error stop "param rejection value incorrect"
      end select

      if (cfg_verbose) then
        print '(a)', "*** STAR LIST *** "
        do i = 1, nstar
          print '(i5, 2f8.2, 1x, 2f8.2, 2x, e10.2, f8.2, 1x, 2f8.2, A3)', &
            i, list(i)%x, list(i)%y, list(i)%ix, list(i)%iy, &
            list(i)%flux, list(i)%rms, list(i)%asymmetry, list(i)%kurtosis, &
            merge(' ', 'X', outlier_mask(i))
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

  !----------------------------------------------------------------------------!

  subroutine cleanup_assymetric_outliers_relative(list, mask)

      type(source_t), intent(in) :: list(:)
      logical(c_bool), intent(out) :: mask(:)
      logical(c_bool), allocatable :: new_mask(:)
      real(r64_k), allocatable :: moments(:,:)
      integer :: n_star
      integer :: i
      character(*), parameter :: fmt1 = '("iter =", i3, 3x, "asymm = ", f5.3, 3x, "max = ", f5.3)'

      n_star = size(list)
      allocate(moments(n_star, 3))
      moments(:,1) = list%asymmetry_uv
      moments(:,2) = list%asymmetry_xy
      moments(:,3) = list%kurtosis

      if (cfg_verbose) write (0, *) '-- OUTLIER CLEANUP'

      allocate(new_mask(n_star))

      mask(:) = .true.

      remove_outliers: do i = 1, n_star

        call asymmetry_test(list, moments, mask, new_mask)

        if (cfg_verbose) write (0, '(a,i0,a,i0,a,i0)') 'stars accepted in iteration ', i, &
          ': ', count(new_mask), "/", size(new_mask)
        if (all(mask .eqv. new_mask)) return

        mask(:) = new_mask(:)

      end do remove_outliers

    end subroutine

  !----------------------------------------------------------------------------!

    subroutine asymmetry_test(list, moments, mask, mask_out)

      type(source_t) :: list(:)
      logical(c_bool) :: mask(:), mask_out(:)
      real(r64_k) :: moments(:,:)
      integer :: n_star, n_star_mask
      real(r64_k) :: asymmetry_av(size(moments, 2)), asymmetry_sd(size(moments, 2)), asymmetry_av_dev
      real(r64_k), allocatable :: asymmetry_dev(:)
      integer :: i

      n_star = size(list)
      n_star_mask = count(mask)

      if (.not. any(mask)) then
        mask_out(:) = .false.
        return
      end if

      allocate(asymmetry_dev(n_star), &
        source=real(0, kind=kind(asymmetry_dev)))

      do i = 1, size(moments, 2)
        asymmetry_av(i) = sum(moments(:,i), mask) / n_star_mask
        asymmetry_sd(i) = sqrt(sum((moments(:,i) - asymmetry_av(i))**2, mask) / n_star_mask)
        asymmetry_dev(:) = asymmetry_dev(:) + ((moments(:,i) - asymmetry_av(i)) / asymmetry_sd(i))**2
      end do

      asymmetry_dev(:) = sqrt(asymmetry_dev(:) / size(moments, 2))

      if (cfg_verbose) then
        print '(a, i5, a, i5, a, *(f10.3,"+-",f10.3,:,", "))', &
          "average asymmetry: ", n_star_mask, "/", n_star,&
          ":   ", (asymmetry_av(i), asymmetry_sd(i), i = 1, size(moments, 2))
      end if
      
      mask_out(:) = asymmetry_dev < 2.5

    end subroutine

  !----------------------------------------------------------------------------!

    subroutine cleanup_assymetric_outliers_absolute(list, mask)
      logical(c_bool) :: mask(:)
      type(source_t) :: list(:)
      integer :: ix_max
      real(r64_k) :: deviation
      integer :: i
      character(*), parameter :: fmt1 = '("iter =", i3, 3x, "asymm = ", f5.3, 3x, "max = ", f5.3)'

      if (cfg_verbose) write (0, *) '-- OUTLIER CLEANUP'

      mask(:) = .true.

      remove_outliers: do i = 1, size(list)

        ! localize the largest element in the array and remove it
        ix_max = maxloc(list % asymmetry, 1, mask)
        mask(ix_max) = .false.

        ! compute the deviation (ideally, asymmetry = 0)
        deviation = sqrt(sum(list % asymmetry**2, mask) / (count(mask) - 1))

        if (cfg_verbose) write (0, fmt1) i, list(ix_max)%asymmetry, 3 * deviation

        ! if not an outlier, set back to true and exit the loop
        if (list(ix_max)%asymmetry <= 3 * deviation) then
          if (cfg_verbose) write (0, *) '-- outlier cleanup finished'
          mask(ix_max) = .true.
          exit remove_outliers
        end if

      end do remove_outliers


    end subroutine



  !----------------------------------------------------------------------------!

    subroutine register_stars(im, list, limit)
      use convolutions, only: convol_fix
      use kernels, only: mexhakrn_alloc
  
      real(buf_k), intent(in), contiguous :: im(:,:)
      type(source_t), intent(out), allocatable :: list(:)
      real(buf_k), allocatable :: im2(:,:), krn(:,:)
      integer, intent(in), optional :: limit
      integer(c_int64_t) :: limit_, nstar
      integer(c_int64_t), parameter :: rslice = 32, margin = 5
      type(findstar_param_t) :: param
  
      limit_ = 256
      if (present(limit)) limit_ = limit
      
      allocate(list(limit_))
      call register_stars_core(im, size(im, 1, c_size_t), size(im, 2, c_size_t), list, limit_, param, nstar)
  
      list = list(:nstar)
    end subroutine
  
  !----------------------------------------------------------------------------!

    subroutine register_stars_c(imd, list, limit, param, nstar) bind(C, name="register_stars")
      use aquila_c_binding
  
      type(buffer_descriptor_t) :: imd
      integer(c_int64_t), intent(in), value :: limit
      type(findstar_param_t) :: param
      type(source_t), intent(out) :: list(limit)
      integer(c_int64_t), intent(out) :: nstar

      real(buf_k), pointer, contiguous :: im(:,:)

      im => from_descriptor(imd)
      call register_stars_core(im, imd%rows, imd%cols, list, limit, param, nstar)

    end subroutine

    subroutine register_stars_core(im, ni, nj, list, limit, param, nstar) bind(C)
      use convolutions, only: convol_fix
      use kernels, only: mexhakrn_alloc, gausskrn_alloc
      use statistics, only: avsd
  
      integer(c_int64_t), intent(in), value :: ni, nj, limit
      type(findstar_param_t) :: param
      real(buf_k), dimension(:,:), intent(in) :: im(ni,nj)
      type(source_t), intent(out) :: list(limit)
      integer(c_int64_t), intent(out) :: nstar
      logical(c_bool), allocatable :: master_mask(:,:)
      real(buf_k) :: av, sd
  
      real(buf_k), dimension(:,:), allocatable :: im2(:,:), im3(:,:), krn(:,:)
  
      krn = mexhakrn_alloc(real(param%blur_radius, r64_k))
  
      allocate(im2(size(im,1), size(im,2)))
      allocate(im3(size(im,1), size(im,2)))
      allocate(master_mask(size(im,1), size(im,2)))
  
      call fix_hot_median(im, im2)
      call convol_fix(im2, krn, im3, 'r')
      where (im3 < 0) im3 = 0
  
      sd = sum(abs(im3)) / size(im3)
      master_mask(:,:) = (im3 > param%thresh_sd * sd)
      call mask_margins_c(master_mask, ni, nj, param%margin)
  
      call aqfindstar(im3, master_mask, ni, nj, list, limit, param, nstar)
    
    end subroutine

end module findstar
