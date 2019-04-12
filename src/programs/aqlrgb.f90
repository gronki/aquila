program aqlrgb

  use globals
  use framehandling, only: image_frame_t, add_suffix
  use convolutions, only: convol_fix
  use kernels, only: gausskrn_alloc

  implicit none
  character(len = 256) :: outfn = "lrgb.fits"
  character(len = 256), allocatable :: fnames(:)
  logical :: cfg_equalize = .false.
  logical :: cfg_color_smooth = .false.
  logical :: cfg_save_cube = .false.
  real(fp) :: smooth_fwhm = 1.5

  call greeting('aqlrgb')

  if (command_argument_count() == 0) then
    call print_help(); stop
  end if

  parse_cli: block
    integer :: i
    character(len = 256) :: arg, buf
    logical :: skip = .false.

    allocate(fnames(0))

    do i = 1, command_argument_count()
      if (skip) then
        skip = .false.; cycle
      end if
      call get_command_argument(i, arg)
      select case (arg)
      case ('-o', '-O', '-output')
        call get_command_argument(i + 1, buf)
        if (buf == "" .or. buf(1:1) == '-') error stop "file name expected"
        outfn = buf
        skip = .true.
      case ('-blur', '-smooth')
        cfg_color_smooth = .true.
        call get_command_argument(i + 1, buf)
        if (buf /= "" .and. buf(1:1) /= '-') then
          read (buf, *) smooth_fwhm
          skip = .true.
        end if
      case ('-wb', '-autowb', '-equalize')
        cfg_equalize = .true.
      case ('-cube', '-3')
        cfg_save_cube = .true.
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

    if (size(fnames) == 4) call frame_l % read_fits(fnames(1))
    call frame_r % read_fits(fnames(merge(1, 2, size(fnames) == 3)))
    call frame_g % read_fits(fnames(merge(2, 3, size(fnames) == 3)))
    call frame_b % read_fits(fnames(merge(3, 4, size(fnames) == 3)))

    if (cfg_color_smooth) then
      perform_color_smooth: block
        real(fp), dimension(:,:), allocatable :: krn, buf

        if (.not. associated(frame_l % data)) then
          frame_l = frame_r % data + frame_g % data + frame_b % data
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

    if (cfg_equalize) then
      perform_equalize: block
        real(fp), dimension(:,:), allocatable :: combined
        logical, dimension(:,:), allocatable :: mask
        integer :: i, j, sz(2)
        integer, parameter :: margin = 64
        real(fp) :: coeff

        combined = (frame_r % data + frame_g % data + frame_b % data) / 3

        mask = (combined > (sum(combined) / size(combined)))
        sz = shape(mask)
        mask(1:margin, :) = .false.
        mask(sz(1) - margin + 1:, :) = .false.
        mask(:, 1:margin) = .false.
        mask(:, sz(2) - margin + 1:) = .false.

        associate (x => frame_g % data, y => frame_r % data)
          coeff = sum(x * y, mask) / sum(x**2, mask)
          print '("R:G = ", f8.3)', coeff
          y = y / coeff
        end associate
        associate (x => frame_g % data, y => frame_b % data)
          coeff = sum(x * y, mask) / sum(x**2, mask)
          print '("B:G = ", f8.3)', coeff
          y = y / coeff
        end associate
      end block perform_equalize
    end if

    if (associated(frame_l % data)) then
      do_lrgb: associate (corr => (frame_l % data) / (frame_r % data + frame_g % data + frame_b % data))
        frame_r % data(:,:) = corr * frame_r % data(:,:)
        frame_g % data(:,:) = corr * frame_g % data(:,:)
        frame_b % data(:,:) = corr * frame_b % data(:,:)
      end associate do_lrgb
    end if

    if (cfg_save_cube) then
      stop "not implemented"
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

  subroutine print_help
    character(len = *), parameter :: fmt = '(a28, 2x, a)', fmt_ctd = '(30x, a)'
    write (*, '(a)') 'prepares the aligned images for RGB processing'
    write (*, '(a)') 'usage: aqlrgb [L] R G B [-o FILE] [options]'
    write (*, '(a)') 'R, G, B are color frames and L is optional luminance'
    write (*, fmt) '-o/-O/-output', 'specifies the output file name prefix'
    write (*, fmt_ctd) 'for example, if image.fits is give, three files'
    write (*, fmt_ctd) 'image.r.fits, image.g.fits, image.b.fits will be written'
    write (*, fmt) '-blur/-smooth [FWHM]', 'smoothes color while preserving luminance'
    write (*, fmt_ctd) 'if FWHM not given, default value (1.5) will be used'
    write (*, fmt) '-[auto]wb/-equalize', 'attempt to make stars white'
    write (*, fmt_ctd) '(only works if background is small)'
    write (*, fmt) '-h[elp]', 'prints help'
  end subroutine


end program aqlrgb
