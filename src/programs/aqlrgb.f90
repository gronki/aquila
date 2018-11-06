program aqlrgb

  use globals
  use framehandling, only: image_frame_t, add_suffix

  implicit none
  character(len = 256) :: outfn = "lrgb.fits"
  character(len = 256), allocatable :: fnames(:)
  logical :: cfg_equalize = .false.

  write (*, '(10x,a)') '*** AQUILA v.' // version // ' ***'

  if (command_argument_count() == 0) then
    call print_help(); stop
  end if

  parse_cli: block
    integer :: i, nargs
    character(len = 256) :: arg
    logical :: skip = .false.

    nargs = command_argument_count()
    allocate(fnames(0))

    do i = 1, nargs
      if (skip) then
        skip = .false.; cycle
      end if
      call get_command_argument(i, arg)
      select case (arg)
      case ('-o', '-O', '-output')
        if (i == nargs) error stop "file name expected"
        call get_command_argument(i + 1, outfn)
        skip = .true.
      case ('-wb', '-autowb', '-equalize')
        cfg_equalize = .true.
      case ("-h", "-help")
        call print_help(); stop
      case default
        if (arg(1:1) == '-') error stop "unknown option: " // trim(arg)
        fnames = [fnames, arg]
      end select
    end do
  end block parse_cli

  if (size(fnames) < 3 .or. size(fnames) > 4) then
    call print_help(); error stop "Incorrect use."
  end if

  do_rgb: block
    type(image_frame_t) :: frame_r, frame_g, frame_b
    character(len = 256) :: outfn_suff

    call frame_r % read_fits(fnames(merge(1, 2, size(fnames) == 3)))
    call frame_g % read_fits(fnames(merge(2, 3, size(fnames) == 3)))
    call frame_b % read_fits(fnames(merge(3, 4, size(fnames) == 3)))

    if (cfg_equalize) then
      perform_equalize: block
        real(fp), dimension(:,:), allocatable :: combined
        logical, dimension(:,:), allocatable :: mask
        integer :: i, j, sz(2)
        integer, parameter :: margin = 16
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

    if (size(fnames) == 4) then
      do_lrgb: block
        type(image_frame_t) :: frame_l
        real(fp), allocatable :: corr(:,:)
        call frame_l % read_fits(fnames(1))
        corr = (frame_l % data) / (frame_r % data + frame_g % data + frame_b % data)
        frame_r % data(:,:) = corr * frame_r % data(:,:)
        frame_g % data(:,:) = corr * frame_g % data(:,:)
        frame_b % data(:,:) = corr * frame_b % data(:,:)
      end block do_lrgb
    end if

    call add_suffix(outfn, '.r', outfn_suff)
    call frame_r % write_fits(outfn_suff)
    call add_suffix(outfn, '.g', outfn_suff)
    call frame_g % write_fits(outfn_suff)
    call add_suffix(outfn, '.b', outfn_suff)
    call frame_b % write_fits(outfn_suff)
  end block do_rgb

contains

  subroutine print_help()
  end subroutine


end program aqlrgb
