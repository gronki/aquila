program aqlrgb

  use globals
  use framehandling
  use convolutions, only: convol_fix
  use kernels, only: gausskrn_alloc

  implicit none
  character(len = 256) :: outfn = "lrgb.fits"
  character(len = 16) :: curve = "", sharpen = ''
  logical :: is_lrgb = .false.
  character(len = 256), allocatable :: fnames(:)
  logical :: cfg_equalize = .false.
  logical :: cfg_color_smooth = .false.
  logical :: cfg_save_cube = .true.
        integer :: margin = 64
        logical :: cfg_background = .false.
  logical :: cfg_transf_lum = .false.
  real(fp) :: smooth_fwhm = 2.0, curve_param = 3.
        real(fp) :: sharpen_strngth = 0.5, sharpen_fwhm = 1.3
        integer :: nx, ny, nc

  call greeting('aq' // cf('l','1') // cf('r','1;91') // cf('g','1;92') // cf('b','1;94'))

  if (command_argument_count() == 0) then
    call print_help(); stop
  end if

  parse_cli: block
    integer :: i, errno
    character(len = 256) :: arg, buf
    integer :: skip

    skip = 0

    do i = 1, command_argument_count()

      if (skip > 0) then
        skip = skip - 1; cycle
      end if

      call get_command_argument(i, arg)

      select case (arg)

      case ('-o', '-output')
        call get_command_argument(i + 1, buf)
        if (buf == "" .or. buf(1:1) == '-') error stop "file name expected"
        outfn = buf
        skip = 1

      case ('-smooth')
        cfg_color_smooth = .true.
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) smooth_fwhm
        if (errno == 0) skip = 1

      case ('-wb', '-equalize')
        cfg_equalize = .true.

      case ('-lin', '-linear')
        curve = ''

      case ('-asinh')
        curve = 'asinh'
        cfg_background = .true.
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) curve_param
        if (errno == 0) skip = 1

      case ('-asinh2')
        curve = 'asinh'
        cfg_transf_lum = .true.
        cfg_background = .true.
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) curve_param
        if (errno == 0) skip = 1

      case ('-sqrt')
        curve = 'sqrt'
        cfg_background = .true.
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) curve_param
        if (errno == 0) skip = 1

      case ('-sqrt2')
        curve = 'sqrt'
        cfg_transf_lum = .true.
        cfg_background = .true.
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) curve_param
        if (errno == 0) skip = 1

      case ('-log')
        curve = 'log'
        cfg_background = .true.
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) curve_param
        if (errno == 0) skip = 1

      case ('-log2')
        curve = 'log'
        cfg_transf_lum = .true.
        cfg_background = .true.
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) curve_param
        if (errno == 0) skip = 1

      case ('-split')
        cfg_save_cube = .false.

      case ('-bg', '-background')
        cfg_background = .true.

      case ('-best')
        curve = 'asinh'
        cfg_background = .true.
        cfg_equalize = .true.
        sharpen = 'deconv'
        cfg_color_smooth = .true.

      case ('-deconv', '-unsharp', '-wavelet')
        sharpen = arg(2:)

        select case (sharpen)
        case ('deconv')
          sharpen_strngth = 0.8
        case ('unsharp')
          sharpen_strngth = 0.4
        case ('wavelet')
          sharpen_strngth = 0.1
        end select

        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) sharpen_fwhm
        if (errno == 0) skip = 1

        if (sharpen_fwhm < 0.1 .or. sharpen_fwhm > 200) error stop 'sharpen_fwhm'

        call get_command_argument(i + 2, buf)
        read (buf, *, iostat = errno) sharpen_strngth
        if (errno == 0) skip = 2

        if (sharpen_strngth <= 0 .or. sharpen_strngth > 1) error stop 'sharpen_strngth'


      case ('-margin')
        call get_command_argument(i + 1, buf)
        read (buf, *, iostat = errno) margin
        if (errno == 0) skip = 1

      case ("-h", "-help")
        call print_help(); stop

      case default
        if (arg(1:1) == '-') error stop "unknown option: " // trim(arg)
        if (allocated(fnames)) then
          fnames = [fnames, arg]
        else
          fnames = [arg]
        end if
      end select
    end do
  end block parse_cli

  if (size(fnames) < 3 .or. size(fnames) > 4) then
    call print_help(); error stop "Incorrect number of files (must be 3 or 4)."
  end if
  is_lrgb = (size(fnames) == 4)

  do_rgb: block
    type(image_frame_t) :: frames(4)
    integer :: nch, ich

    associate(frame_l => frames(4), frame_r=> frames(1), frame_g => frames(2), frame_b => frames(3))

    call read_fits_naxes(fnames(1), nx, ny)
    nch = merge(4, 3, is_lrgb)
    
    if (is_lrgb) call frame_l % read_fits(fnames(1))
    call frame_r % read_fits(fnames(merge(2, 1, is_lrgb)))
    call frame_g % read_fits(fnames(merge(3, 2, is_lrgb)))
    call frame_b % read_fits(fnames(merge(4, 3, is_lrgb)))

    do ich = 1, nch
      where (.not. ieee_is_normal(frames(ich) % data)) frames(ich) % data = 0
    end do

    if (cfg_equalize .or. cfg_background) then
      perform_equalize: block
        use statistics, only: outliers, avsd
        use ieee_arithmetic, only: ieee_is_normal
        logical, dimension(:,:), allocatable :: mask, maskbg
        real(fp), dimension(:,:), allocatable :: L
        integer :: i, j
        real(fp) :: coeff, av, sd, bg_off
        real(fp), dimension(nch) :: bg, sg
        real(fp), parameter :: a = 2.0, b = 0.5


        allocate(mask(nx, ny), source=.true.)
        mask(1:margin, :) = .false.
        mask(nx - margin + 1:, :) = .false.
        mask(:, 1:margin) = .false.
        mask(:, ny - margin + 1:) = .false.
        maskbg = mask

        L = Lum(frame_r % data, frame_g % data, frame_b % data)

        call outliers(L, maskbg, 3._fp, 32, av, sd)
        ! maskbg = mask .and. (L < av + 2 * sd)
        mask = mask .and. (L > av + 3 * sd)
        ! cube(:,:,4) = thrfun((L - av) / sd - 4)

        deallocate(L)

        do i = 1, nch
          call avsd(frames(i) % data, maskbg, av, sg(i))
          bg(i) = av - a * sg(i)
        end do

        bg_off = sum(bg) / size(bg) * (1 - b)

        write(*, '("stars ", f4.1, "% surface, background ", f4.1, "%")') &
        &     100 * real(count(mask)) / size(mask),     &
        &     100 * real(count(maskbg)) / size(maskbg)

        write(*, '(a10, " = ", *(f6.1))') 'background', bg
        write(*, '(a10, " = ", *(f6.1))') 'sigma', sg

        if ( cfg_background ) then
          do ich = 1, 3
            frames(ich) % data = frames(ich) % data - (bg(ich) - bg_off)
          end do

          do i = 1, nch
            ! frames(i) % data = frames(i) % data - (bg(i) - bg_off)
            call avsd(frames(i) % data, maskbg, av, sg(i))
            bg(i) = av - a * sg(i)
          end do

          write(*, '(a10, " = ", *(f6.1))') 'background', bg
          write(*, '(a10, " = ", *(f6.1))') 'sigma', sg

        end if

        if (cfg_equalize) then
          associate (x => frame_g % data - bg(2), y => frame_r % data - bg(1))
            coeff = sum(x * y, mask) / sum(x**2, mask)
            print '("R:G = ", f8.3)', coeff
            frame_r % data = bg(1) + y / coeff
          end associate

          associate (x => frame_g % data - bg(2), y => frame_b % data - bg(3))
            coeff = sum(x * y, mask) / sum(x**2, mask)
            print '("B:G = ", f8.3)', coeff
            frame_b % data = bg(3) + y / coeff
          end associate
        end if
      end block perform_equalize
    end if

    if (cfg_color_smooth) then
      perform_color_smooth: block
        real(fp), dimension(:,:), allocatable :: krn, buf

        if (.not. allocated(frame_l % data)) then
          frame_l % data = Lum(frame_r % data, frame_g % data, frame_b % data)
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

    if ( sharpen /= '' ) then
      do_unsharp_lum: block
        use kernels, only: mexhakrn_alloc, gausskrn_alloc, print_kernel
        use convolutions, only: convol_fix
        use deconvolutions, only: deconvol_lr

        real(fp), allocatable :: krn(:,:), lum2(:,:)
        integer :: i

        if (.not. allocated(frame_l % data)) then
          frame_l%data = Lum(frame_r % data, frame_g % data, frame_b % data)
        end if
        allocate(lum2, mold = frame_l % data)

        select case (sharpen)
        case ('wavelet')
          krn = mexhakrn_alloc(sharpen_fwhm)
          call convol_fix(frame_l % data, krn, lum2, 'r')
          frame_l % data(:,:) = sharpen_strngth * lum2 + (1 - sharpen_strngth) * (frame_l % data)
        case ('deconv')
          krn = gausskrn_alloc(sharpen_fwhm)
          call deconvol_lr(frame_l % data, krn, sharpen_strngth, 99, lum2)
          frame_l % data(:,:) = lum2
        case ('unsharp')
          krn = gausskrn_alloc(sharpen_fwhm)
          call convol_fix(frame_l % data, krn, lum2, 'r')
          lum2(:,:) = frame_L % data - lum2 / sum(krn)
          frame_l % data(:,:) = sharpen_strngth * lum2 + (1 - sharpen_strngth) * (frame_l % data)
        case default
          error stop 'wrong sharpen mode'
        end select

#       if _DEBUG
        call print_kernel(krn)
#       endif

      end block do_unsharp_lum
    end if

    ! if (curve /= '' .and. cfg_transf_lum) then
    !   if (.not. allocated(frame_l % data)) then
    !     frame_l = Lum(frame_r % data, frame_g % data, frame_b % data)
    !   end if
    ! end if

    if (allocated(frame_l % data)) then
      do_lrgb: block
        real(fp), allocatable :: x(:,:)
        x = (frame_l % data) / Lum(frame_r % data, frame_g % data, frame_b % data)
        frame_r % data(:,:) = x * frame_r % data(:,:)
        frame_g % data(:,:) = x * frame_g % data(:,:)
        frame_b % data(:,:) = x * frame_b % data(:,:)
      end block do_lrgb
    end if

    ! at this point the luminance stops being relevant (For now)

    if (curve /= '') then
      do_curve: block
        use statistics, only: avsd, outliers

        integer :: i
        real(fp), allocatable :: x(:,:), y(:,:)
        real(fp) :: a, b, av, sd

        write(*, '("performing ",a," curve transform on the image: ",a)') &
            trim(merge('luminance', 'color    ', cfg_transf_lum)), curve

        if (allocated(frame_l % data)) then
          x = frame_l % data
        else
          x = Lum(frame_r % data, frame_g % data, frame_b % data)
        end if

        call outliers(x(1+margin:ny-margin, 1+margin:ny-margin), 3._fp, 10, av, sd)
        b = av - curve_param * sd
        a = curve_param * sd 

        if ( cfg_transf_lum ) then
          curve_lum: block
            allocate(y, mold = x)
            call apply_curve(curve, x, a, b, y)
            x = y / x
            deallocate(y)
            do i = 1, 3
              frames(i) % data = frames(i) % data * x
            end do
          end block curve_lum
        else
          do i = 1, 3
            call apply_curve(curve, frames(i) % data, a, b, frames(i) % data)
          end do
        end if
      end block do_curve
    end if

    if (cfg_save_cube) then
      block
        use png

        real(fp) :: vmin, vmax, av, sd
        real(fp), allocatable :: l(:,:), cube(:,:,:)

        allocate(cube(nx, ny, 3))

        do ich = 1, 3
          cube(:, :, ich) = frames(ich) % data
        end do

        if (endswith(outfn, '.png')) then

          l = Lum(frame_r % data(64:nx-64,64:ny-64), &
          &   frame_g % data(64:nx-64,64:ny-64),     &
          &   frame_b % data(64:nx-64,64:ny-64))
          av = sum(l) / size(l)
          sd = sqrt(sum((l - av)**2) / (size(l) - 1))
          deallocate(l)

          vmin = av - 1.5 * sd
          vmax = av + 9.5 * sd

          call write_png(outfn, (cube(:,:,1:3) - vmin) / (vmax - vmin))
          call write_fits_3d(replace_extn(outfn, 'fits'), cube(:,:,1:3))
        else
          call write_fits_3d(outfn, cube(:,:,1:3))
        end if
      end block
    else
      call frame_r % write_fits(add_suffix(outfn, '.r'))
      call frame_g % write_fits(add_suffix(outfn, '.g'))
      call frame_b % write_fits(add_suffix(outfn, '.b'))
    end if
  end associate
  end block do_rgb

contains

  elemental real(fp) function Lum(R,G,B) result(L)
    real(fp), intent(in) :: R, G, B
    real(fp), parameter :: wr = 0.9, wg = 1.1, wb = 0.7
    L = (wr * R + wg * G + wb * B) / (wr + wg + wb)
  end function

  elemental subroutine apply_curve(tt, x, a, b, y)
    character(len = *), intent(in) :: tt
    real(fp), intent(in) :: x, a, b
    real(fp), intent(out) :: y
    real(fp) :: x1

    x1 = merge(x, b, x >= b)

    select case (tt)
    case ('sqrt')
      y = sqrt(1 + 2 * (x1 - b) / a) - 1
    case ('asinh')
      y = asinh((x1 - b) / a)
    case ('log')
      y = log(1 + (x1 - b) / a)
    case default
      error stop 'curve?'
    end select
  end subroutine

  subroutine print_help
    use globals, only: hlp_fmt, hlp_fmtc, fmthlp
    write (*, '(a)') 'prepares the aligned images for RGB processing'
    write (*, '(a)') 'usage: aqlrgb [L] R G B [-o FILE] [options]'
    write (*, '(a)') 'R, G, B are color frames and L is optional luminance'
    write (*, hlp_fmt) '-o/-output', 'specifies the output file name'
    write (*, hlp_fmtc) '(allowed formats: fits, png)'
    write (*, hlp_fmt) '-split', 'save as 3 files fits rather than one cube'
    write (*, hlp_fmtc) 'for example, if image.fits is given to -o, three files'
    write (*, hlp_fmtc) 'image.r.fits, image.g.fits, image.b.fits will be written'
    write (*, hlp_fmt) '-smooth [FWHM]', 'smoothes color while preserving luminance'
    write (*, hlp_fmtc) 'if FWHM not given, default value (2.5) will be used'
    write (*, hlp_fmt) '-wb/-equalize', 'attempt to make stars white'
    write (*, hlp_fmtc) '(works best if background is small)'
    write (*, hlp_fmt) '-bg/-background', 'attempt to make background black'
    write (*, hlp_fmtc) '(do not use for strong nebulosity)'
    write (*, hlp_fmt) '-sqrt/-asinh/-log', 'compress the image levels before saving'
    write (*, hlp_fmt) '-sqrt2/-asinh2/-log2', 'same but using luminosity'
    write (*, hlp_fmtc) '(boosts star colors but can kill some details)'
    write (*, '(a)') '-sharpen/wavelet/deconv [FWHM [strength]]'
    write (*, hlp_fmtc) 'sharpen the luminance (default strength=0.85)'
    print fmthlp, '-margin W', 'sets width for processing margin in px {def.: 64}'
    write (*, hlp_fmt) '-h[elp]', 'prints help'
  end subroutine

  subroutine write_fits_3d(fn, cube, errno)
    use framehandling, only: frame_t
    use iso_fortran_env, only: real32
    ! class(frame_t), intent(in) :: frame_r, frame_g, frame_b
    real(fp), intent(in) :: cube(:,:,:)
    character(len = *), intent(in) :: fn
    integer, intent(inout), optional :: errno
    integer :: ftiostat, un, iostat
    real(real32), allocatable :: cube2(:,:,:)

    iostat = 0
    open (99, file = fn, status = 'old', iostat = iostat)
    if (iostat == 0) then
      write(*, '("file ",a," exists, deleting...")') trim(fn)
      close (99, status = 'delete')
    end if

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

    cube2 = real(cube, real32)
    call ftphps(un, -32, 3, shape(cube2), ftiostat)
    call ftppre(un, 1, 1, size(cube2), cube2, ftiostat)

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
