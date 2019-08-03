program aqlrgb

  use globals
  use framehandling, only: image_frame_t, add_suffix
  use convolutions, only: convol_fix
  use kernels, only: gausskrn_alloc

  implicit none
  character(len = 256) :: outfn = "lrgb.fits"
  character(len = 16) :: transform = ""
  character(len = 256), allocatable :: fnames(:)
  logical :: cfg_equalize = .false.
  logical :: cfg_color_smooth = .false.
  logical :: cfg_save_cube = .true.
  logical :: cfg_background = .false.
  logical :: cfg_transf_lum = .false.
  real(fp) :: smooth_fwhm = 2.5

  call greeting('aq' // cf('l','1') // cf('r','1;91') // cf('g','1;92') // cf('b','1;94'))

  if (command_argument_count() == 0) then
    call print_help(); stop
  end if

  parse_cli: block
    integer :: i, errno
    character(len = 256) :: arg, buf
    logical :: skip = .false.

    allocate(fnames(0))

    do i = 1, command_argument_count()
      if (skip) then
        skip = .false.; cycle
      end if
      call get_command_argument(i, arg)
      select case (arg)
      case ('-o', '-output')
        call get_command_argument(i + 1, buf)
        if (buf == "" .or. buf(1:1) == '-') error stop "file name expected"
        outfn = buf
        skip = .true.
      case ('-smooth')
        cfg_color_smooth = .true.
        call get_command_argument(i + 1, buf)
        if (buf /= "" .and. buf(1:1) /= '-') then
          read (buf, *, iostat = errno) smooth_fwhm
          if (errno == 0) skip = .true.
        end if
      case ('-wb', '-equalize')
        cfg_equalize = .true.
      case ('-asinh')
        transform = 'asinh'
      case ('-asinh2')
        transform = 'asinh'
        cfg_transf_lum = .true.
      case ('-sqrt')
        transform = 'sqrt'
      case ('-sqrt2')
        transform = 'sqrt'
        cfg_transf_lum = .true.
      case ('-log')
        transform = 'log'
      case ('-log2')
        transform = 'log'
        cfg_transf_lum = .true.
      case ('-split')
        cfg_save_cube = .false.
      case ('-bg', '-background')
        cfg_background = .true.
      case ('-best')
        transform = 'asinh'
        cfg_background = .true.
        cfg_equalize = .true.
        cfg_color_smooth = .true.
        smooth_fwhm = 3
      case ("-h", "-help")
        call print_help(); stop
      case default
        if (arg(1:1) == '-') error stop "unknown option: " // trim(arg)
        fnames = [fnames, arg]
      end select
    end do
  end block parse_cli

  if (size(fnames) < 3 .or. size(fnames) > 4) then
    call print_help(); error stop "Incorrect number of files (must be 3 or 4)."
  end if

  do_rgb: block
    type(image_frame_t) :: frame_r, frame_g, frame_b, frame_l
    logical :: with_luminance

    with_luminance = (size(fnames) == 4)

    if (with_luminance) call frame_l % read_fits(fnames(1))
    call frame_r % read_fits(fnames(merge(2, 1, with_luminance)))
    call frame_g % read_fits(fnames(merge(3, 2, with_luminance)))
    call frame_b % read_fits(fnames(merge(4, 3, with_luminance)))

    if (any(shape(frame_g % data) /= shape(frame_r % data)) &
    & .or. any(shape(frame_g % data) /= shape(frame_b % data))) then
      error stop "color image frame size mismatch"
    end if

    if (with_luminance) then
      if (any(shape(frame_g % data) /= shape(frame_l % data))) then
        error stop "color frame size must match luminance"
      end if
    end if

    if (cfg_equalize .or. cfg_background) then
      perform_equalize: block
        use statistics, only: outliers, sigstd
        use ieee_arithmetic, only: ieee_is_normal
        logical, dimension(:,:), allocatable :: mask, maskbg
        real(fp), dimension(:,:), allocatable :: L
        integer :: i, j, sz(2)
        integer, parameter :: margin = 64
        real(fp) :: coeff, mean, stdev, bg(3)

        mask = ieee_is_normal(frame_g % data)
        allocate(maskbg, mold = mask)
        sz = shape(mask)
        mask(1:margin, :) = .false.
        mask(sz(1) - margin + 1:, :) = .false.
        mask(:, 1:margin) = .false.
        mask(:, sz(2) - margin + 1:) = .false.

        L = Lum(frame_r % data, frame_g % data, frame_b % data)

        call outliers(L, 3.0_fp, 24, maskbg)
        call sigstd(L, mean, stdev, maskbg)
        maskbg = maskbg .and. mask
        mask = mask .and. (L > mean + 5 * stdev)

        associate (nbg => count(maskbg))
          bg(1) = sum(frame_r % data, maskbg) / nbg
          bg(2) = sum(frame_g % data, maskbg) / nbg
          bg(3) = sum(frame_b % data, maskbg) / nbg
        end associate

        write (0, '("stars ", f4.1, "% surface, background ", f4.1, "%")') &
        &     100 * real(count(mask)) / size(mask),     &
        &     100 * real(count(maskbg)) / size(maskbg)

        write (0, '("' // cf('background', '93') // ' R,G,B = ", 3f6.1)') bg

        if ( cfg_background ) then
          write (0, '("bkg R-G = ", f8.1)') bg(1) - bg(2)
          frame_r % data = frame_r % data - (bg(1) - bg(2))
          write (0, '("bkg B-G = ", f8.1)') bg(3) - bg(2)
          frame_b % data = frame_b % data - (bg(3) - bg(2))
          associate (nbg => count(maskbg))
            bg(1) = sum(frame_r % data, maskbg) / nbg
            bg(2) = sum(frame_g % data, maskbg) / nbg
            bg(3) = sum(frame_b % data, maskbg) / nbg
          end associate
        end if

        if (cfg_equalize) then
          associate (x => frame_g % data - bg(2), y => frame_r % data - bg(1))
            coeff = sum(x * y, mask) / sum(x**2, mask)
            print '("R:G = ", f8.3)', coeff
            frame_r % data = bg(1) + (frame_r % data - bg(1)) / coeff
          end associate

          associate (x => frame_g % data - bg(2), y => frame_b % data - bg(3))
            coeff = sum(x * y, mask) / sum(x**2, mask)
            print '("B:G = ", f8.3)', coeff
            frame_b % data = bg(3) + (frame_b % data - bg(3)) / coeff
          end associate
        end if
      end block perform_equalize
    end if

    if (cfg_color_smooth) then
      perform_color_smooth: block
        real(fp), dimension(:,:), allocatable :: krn, buf

        if (.not. associated(frame_l % data)) then
          frame_l = Lum(frame_r % data, frame_g % data, frame_b % data)
        end if

        krn = gausskrn_alloc(smooth_fwhm)
        !$omp parallel sections private(buf)
        !$omp section
        buf = frame_r % data
        call convol_fix(buf, krn, frame_r % data, 'e')
        !$omp section
        buf = frame_g % data
        call convol_fix(buf, krn, frame_g % data, 'e')
        !$omp section
        buf = frame_b % data
        call convol_fix(buf, krn, frame_b % data, 'e')
        !$omp end parallel sections
      end block perform_color_smooth
    end if

    if (associated(frame_l % data)) then
      do_lrgb: associate (corr => (frame_l % data) / Lum(frame_r % data, frame_g % data, frame_b % data))
        frame_r % data(:,:) = corr * frame_r % data(:,:)
        frame_g % data(:,:) = corr * frame_g % data(:,:)
        frame_b % data(:,:) = corr * frame_b % data(:,:)
      end associate do_lrgb
    end if

    if (transform /= '') then
      write (0, '("preforming transform on the image: ", a)') transform
      if ( cfg_transf_lum ) then
        write (0, *) 'transform of luminance'
        transform_lum: block
          real(fp), allocatable :: x(:,:), y(:,:)
          x = Lum(frame_r % data, frame_g % data, frame_b % data)
          allocate(y, mold = x)
          call apply_transform(transform, x, y)
          x = y / x
          frame_r % data(:,:) = frame_r % data(:,:) * x
          frame_g % data(:,:) = frame_g % data(:,:) * x
          frame_b % data(:,:) = frame_b % data(:,:) * x
        end block transform_lum
      else
        write (0, *) 'transform of colors'
        call apply_transform(transform, frame_r % data, frame_r % data)
        call apply_transform(transform, frame_g % data, frame_g % data)
        call apply_transform(transform, frame_b % data, frame_b % data)
      end if
    end if

    if (cfg_save_cube) then
      call write_fits_3d(outfn, frame_r, frame_g, frame_b)
    else
      save_3files: block
        character(len = 256) :: outfn_suff
        call add_suffix(outfn, '.r', outfn_suff)
        call frame_r % write_fits(outfn_suff)
        call add_suffix(outfn, '.g', outfn_suff)
        call frame_g % write_fits(outfn_suff)
        call add_suffix(outfn, '.b', outfn_suff)
        call frame_b % write_fits(outfn_suff)
      end block save_3files
    end if

  end block do_rgb

contains

  elemental real(fp) function Lum(R,G,B) result(L)
    real(fp), intent(in) :: R, G, B
    L = R + 1.5 * G + 0.7 * B
    L = L / 3
  end function

  subroutine apply_transform(transf, x, y)
    character(len = 16) :: transf
    real(fp), intent(in) :: x(:,:)
    real(fp), intent(inout) :: y(:,:)
    real(fp), parameter :: a = 300
    select case (transf)
    case ('sqrt')
      y = sqrt(1 + 2 * x / a) - 1
    case ('asinh')
      y = asinh(x / a)
    case ('log')
      y = log(1 + x / a) - 1
    case default
        error stop 'transform?'
    end select
  end subroutine

  subroutine print_help
    character(len = *), parameter :: fmt = '(a28, 2x, a)', fmt_ctd = '(30x, a)'
    write (*, '(a)') 'prepares the aligned images for RGB processing'
    write (*, '(a)') 'usage: aqlrgb [L] R G B [-o FILE] [options]'
    write (*, '(a)') 'R, G, B are color frames and L is optional luminance'
    write (*, fmt) '-o/-output', 'specifies the output file name'
    write (*, fmt) '-split', 'save as 3 files fits rather than one cube'
    write (*, fmt_ctd) 'for example, if image.fits is given to -o, three files'
    write (*, fmt_ctd) 'image.r.fits, image.g.fits, image.b.fits will be written'
    write (*, fmt) '-smooth [FWHM]', 'smoothes color while preserving luminance'
    write (*, fmt_ctd) 'if FWHM not given, default value (2.5) will be used'
    write (*, fmt) '-wb/-equalize', 'attempt to make stars white'
    write (*, fmt_ctd) '(only works if background is small)'
    write (*, fmt) '-sqrt/-asinh/-log', 'compress the image levels before saving'
    write (*, fmt) '-sqrt2/-asinh2/-log2', 'same but using luminosity'
    write (*, fmt) '-h[elp]', 'prints help'
  end subroutine

  subroutine write_fits_3d(fn, frame_r, frame_g, frame_b, errno)
    use framehandling, only: frame_t
    use iso_fortran_env, only: real32
    class(frame_t), intent(in) :: frame_r, frame_g, frame_b
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno
    integer :: ftiostat, un
    real(real32), allocatable :: cube(:,:,:)

    ftiostat = 0

    call ftgiou(un, ftiostat)
    call ftdkinit(un, fn, 1, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "could not create output file: " // trim(fn)
      end if
    end if

    allocate(cube(size(frame_g % data, 1), size(frame_g % data, 2), 3))
    cube(:,:,1) = frame_r % data(:,:)
    cube(:,:,2) = frame_g % data(:,:)
    cube(:,:,3) = frame_b % data(:,:)
    call ftphps(un, -32, 3, shape(cube), ftiostat)
    call ftppre(un, 1, 1, size(cube), cube, ftiostat)
    deallocate(cube)

    call ftclos(un, ftiostat)
    call ftfiou(un, ftiostat)

    if (ftiostat /= 0) then
      call ftrprt("stderr", ftiostat)
      if (present(errno)) then
        errno = ftiostat; return
      else
        error stop "error writing FITS file: " // trim(fn)
      end if
    end if
  end subroutine

end program aqlrgb
