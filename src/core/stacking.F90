module stacking

  use globals
  use iso_c_binding
  implicit none (type, external)

contains

  !----------------------------------------------------------------------------!

  subroutine register_stars(im, list, limit)
    use convolutions, only: convol_fix
    use kernels, only: mexhakrn_alloc
    use findstar, only: aqfindstar, source_t

    real(fp), intent(in), contiguous :: im(:,:)
    type(source_t), intent(out), allocatable :: list(:)
    real(fp), allocatable :: im2(:,:), krn(:,:)
    integer, intent(in), optional :: limit
    integer(c_int64_t) :: limit_, nstar
    integer(c_int64_t), parameter :: rslice = 32, margin = 5

    limit_ = 1024
    if (present(limit)) limit_ = limit
    
    allocate(list(limit_))
    call register_stars_f(im, size(im, 1, c_size_t), size(im, 2, c_size_t), list, limit_, rslice, &
      margin, 2._c_double, nstar)

    list = list(:nstar)
  end subroutine

  subroutine fix_hot_median(im, im2)
    real(fp) :: im(:,:), im2(:,:), av, sd
    integer :: i, j, i1, i2, j1, j2, n1, n2

    n1 = size(im, 1)
    n2 = size(im, 2)

    do concurrent(i = 1:n1, j=1:n2)
      if (i == 1 .or. i == n1 .or. j == 1 .or. j == n2) then
        im2(i,j) = im(i,j)
        cycle
      end if
      av = im(i-1, j-1) + im(i, j-1)  + im(i+1, j-1) &
      &  + im(i-1, j  )               + im(i+1, j  ) &
      &  + im(i-1, j+1) + im(i, j+1)  + im(i+1, j+1)
      av = av / 8
      sd = im(i-1, j-1)**2 + im(i, j-1)**2  + im(i+1, j-1)**2 &
      &  + im(i-1, j  )**2                  + im(i+1, j  )**2 &
      &  + im(i-1, j+1)**2 + im(i, j+1)**2  + im(i+1, j+1)**2
      sd = sd / 8
      sd = sqrt(max(0._fp, sd - av * av)) 
      im2(i,j) = merge(im(i, j), av, im(i,j) < av + 3 * sd)
    end do
  end subroutine

  subroutine register_stars_f(im, ni, nj, list, limit, rslice, &
      margin, kernel, nstar) bind(C)
    use convolutions, only: convol_fix
    use kernels, only: mexhakrn_alloc, gausskrn_alloc
    use findstar, only: aqfindstar, source_t
    use frame_m, only: write_fits_quick
    use statistics, only: avsd

    integer(c_int64_t), intent(in), value :: ni, nj, limit
    integer(c_int64_t), intent(in), value :: rslice, margin
    real(fp), dimension(:,:), intent(in) :: im(ni,nj)
    type(source_t), intent(out) :: list(limit)
    integer(c_int64_t), intent(out) :: nstar
    real(c_double), intent(in), value :: kernel
    logical(c_bool), allocatable :: master_mask(:,:)
    real(fp) :: av, sd

    real(fp), dimension(:,:), allocatable :: im2(:,:), im3(:,:), krn(:,:)

    krn =  mexhakrn_alloc(real(kernel, fp)) !+ 0.5 * gausskrn_alloc(real(kernel, fp))

    allocate(im2(size(im,1), size(im,2)))
    allocate(im3(size(im,1), size(im,2)))
    allocate(master_mask(size(im,1), size(im,2)))

    call fix_hot_median(im, im2)
    
#   ifdef _DEBUG
    call write_fits_quick("findstar_im2.fits", im2)  
#   endif  

    call convol_fix(im2, krn, im3, 'r')
    where (im3 < 0) im3 = 0

#   ifdef _DEBUG
    call write_fits_quick("findstar_im3.fits", im3)    
#   endif  

    sd = sqrt(sum(im3**2) / size(im3))
    master_mask(:,:) = (im3 > sd) !.and. (im < 0.9 * maxval(im))

#   ifdef _DEBUG
    call write_fits_quick("findstar_mask.fits", merge(im, 0.0_fp, master_mask))    
#   endif

    call aqfindstar(im3, master_mask, ni, nj, list, limit, rslice, margin, nstar)
  
  end subroutine

  !----------------------------------------------------------------------------!

  pure function check_corners(tx, nx, ny) result(margin)
    use new_align, only: transform_t

    class(transform_t), intent(in) :: tx
    integer, intent(in) :: nx, ny
    real(fp) :: rx, ry, sx, sy
    integer :: margin

    rx = 0.5_fp * (nx - 1)
    ry = 0.5_fp * (ny - 1)

    margin = 0
    call tx % apply(-rx, -ry, sx, sy)
    margin = max(margin, ceiling(abs(abs(rx) - abs(sx))), ceiling(abs(abs(ry) - abs(sy))))
    call tx % apply( rx, -ry, sx, sy)
    margin = max(margin, ceiling(abs(abs(rx) - abs(sx))), ceiling(abs(abs(ry) - abs(sy))))
    call tx % apply( rx,  ry, sx, sy)
    margin = max(margin, ceiling(abs(abs(rx) - abs(sx))), ceiling(abs(abs(ry) - abs(sy))))
    call tx % apply(-rx,  ry, sx, sy)
    margin = max(margin, ceiling(abs(abs(rx) - abs(sx))), ceiling(abs(abs(ry) - abs(sy))))
  end function

  !----------------------------------------------------------------------------!

  subroutine normalize_offset_gain(buffer, margin)
    use statistics, only: linfit

    real(fp), intent(inout) :: buffer(:,:,:)
    integer, intent(in) :: margin
    
    real(fp) :: a, b
    real(fp), allocatable :: imref(:,:), xx(:), yy(:)
    logical, allocatable :: mask(:,:)
    integer :: i, j, np, sz(3), nstack

    sz = shape(buffer)
    nstack = sz(3)

    ! create mean frame to normalize to
    allocate(imref(sz(1), sz(2)))
    do concurrent (i = 1:sz(1), j = 1:sz(2))
      imref(i,j) = sum(buffer(i, j, :)) / nstack
    end do

    ! create mask which excludes edges and the brigtenst pixels
    allocate(mask(sz(1), sz(2)))
    associate (m => margin)
      associate (imc => imref(1+m : sz(1)-m, 1+m : sz(2)-m))
        mask = imref < (minval(imc) + maxval(imc)) / 2
      end associate
      mask(:m, :) = .false.; mask(sz(1)-m+1:, :) = .false.
      mask(:, :m) = .false.; mask(:, sz(2)-m+1:) = .false.
    end associate

    ! pack it into 1-d array
    np = count(mask)
    allocate(xx(np), yy(np))
    xx(:) = pack(imref, mask)
    deallocate(imref)

    do i = 1, nstack
      yy(:) = pack(buffer(:,:,i), mask)
      call linfit(xx, yy, a, b)
      write (stderr, '("NORM frame(",i2,") y = ",f5.3,"x + ",f7.1)') i, a, b
      buffer(:,:,i) = (buffer(:,:,i) - b) / a
    end do
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine stack_frames(strategy, method, frames, buffer, frame_out)
    use framehandling, only: image_frame_t

    character(len = *), intent(in) :: strategy, method
    real(fp), intent(in) :: buffer(:,:,:)
    type(image_frame_t), intent(in) :: frames(:)
    type(image_frame_t), intent(out) :: frame_out
    real(real64) :: t1, t2
    integer :: nstack

    nstack = size(buffer, 3)

    call frame_out % check_shape(size(buffer, 1), size(buffer, 2))

    call cpu_time(t1)
    call stack_buffer(method, buffer(:, :, 1:nstack), frame_out % data)
    call cpu_time(t2)
    print perf_fmt, 'stack', t2 - t1

    write_extra_info_hdr: block

      call frame_out % hdr % add_int('NSTACK', nstack)
      call frame_out % hdr % add_str('STCKMTD', method)
      if (strategy /= '') call frame_out % hdr % add_str('FRAMETYP', strategy)

      call propagate_average_value_real(frames(1:nstack), 'EXPTIME', frame_out, .false.)
      call propagate_average_value_real(frames(1:nstack), 'CCD-TEMP', frame_out, .true.)
      frame_out%exptime = sum(frames(1:nstack)%exptime)

    end block write_extra_info_hdr

    if ((strategy == 'bias' .or. strategy == 'dark') .and. nstack > 1) then
      estimate_noise: block
        real(fp) :: rms

        rms = estimate_differential_noise(buffer)

        write (*, '("RMS = ", f10.2)') rms
        call frame_out % hdr % add_real('RMS', real(rms))
        call frame_out % hdr % add_real('STACKRMS', real(rms / sqrt(1.0_fp * nstack)))
      end block estimate_noise
    end if

    call frame_out % hdr % add('AQLVER', version)

  end subroutine stack_frames

  !----------------------------------------------------------------------------!

  subroutine collect_frames_into_buffer(frames, buffer)
    use framehandling, only: image_frame_t

    class(image_frame_t), intent(in) :: frames(:)
    real(kind=fp), allocatable :: buffer(:,:,:)
    integer :: n_frames, i, ni, nj

    n_frames = size(frames)
    do i = 1, n_frames
      if (.not. allocated(frames(i) % data)) &
        error stop "attempting to collect frames into buffer but one of them is empty"
      if (i == 1) then
          ni = size(frames(i)%data, 1)
          nj = size(frames(i)%data, 2)
          allocate(buffer(ni, nj, n_frames))
      end if
      buffer(:,:,i) = frames(i) % data
    end do
  end subroutine

  !----------------------------------------------------------------------------!

  function select_output_filename(output_fn, strategy) result(output_fn_clean)
    character(len=*), intent(in) :: output_fn, strategy
    character(len=:), allocatable :: output_fn_clean

    if (output_fn == "") then
      if (strategy /= "") then
        output_fn_clean = trim(strategy) // ".fits"
      else
        output_fn_clean = "out.fits"
      end if
    else
      output_fn_clean = output_fn
    end if
  end function

  !----------------------------------------------------------------------------!

  subroutine stack_buffer(method, buffer, buffer_out)
    use statistics, only: quickselect, sigclip2

    real(fp), intent(in) :: buffer(:,:,:)
    real(fp), intent(out) :: buffer_out(:,:)
    character(len = *), intent(in) :: method
    integer :: i, j, nstack
    real(fp) :: a(size(buffer, 3))

    nstack = size(buffer, 3)

    select case (method)
    case ('m', 'median')
      !$omp parallel do private(i, j, a)
      do j = 1, size(buffer, 2)
        do i = 1, size(buffer, 1)
          a(:) = buffer(i, j, 1:nstack)
          ! forall (k = 1:nstack) a(k) = frames(k) % data(i, j)
          buffer_out(i, j) = quickselect(a(:), (nstack + 1) / 2)
        end do
      end do
      !$omp end parallel do
    case ('s', 'sigclip')
      !$omp parallel do private(i, j, a)
      do j = 1, size(buffer, 2)
        do i = 1, size(buffer, 1)
          ! forall (k = 1:nstack) a(k) = frames(k) % data(i, j)
          ! call sigclip2(a(:), frame_out % data(i, j))
          buffer_out(i, j) = sigclip2(buffer(i, j, 1:nstack), 3._fp)
        end do
      end do
      !$omp end parallel do
    case default
      do j = 1, size(buffer, 2)
        do i = 1, size(buffer, 1)
          buffer_out(i,j) = sum(buffer(i, j, 1:nstack)) / nstack
        end do
      end do
    end select
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine propagate_average_value_real(frames, kw, frame_out, average)
    use framehandling, only: image_frame_t

    class(image_frame_t), intent(in) :: frames(:)
    class(image_frame_t), intent(inout) :: frame_out
    logical, intent(in) :: average
    character(len = *) :: kw
    logical :: m(size(frames))
    real :: av
    integer :: i

    m(:) = [ (frames(i) % hdr % has_key(kw), i = 1, size(frames)) ]
    if (count(m) > 0) then
      av = sum([ (merge(frames(i) % hdr % get_real(kw, 0.0), 0.0, m(i)), &
      &     i = 1, size(frames)) ]) / merge(count(m), 1, average)
      call frame_out % hdr % add_real(kw, av)
    end if
  end subroutine

  !----------------------------------------------------------------------------!

  pure function estimate_differential_noise(buffer) result(rms)
    use iso_fortran_env, only: int64
    real(fp), intent(in) :: buffer(:,:,:)
    real(fp) :: rms, av(size(buffer, 3))
    integer :: i, n
    integer(int64) :: nxny

    nxny = size(buffer, 1, kind = int64) * size(buffer, 2, kind = int64)
    n = size(buffer, 3)

    do concurrent (i = 1:n)
      av(i) = sum(buffer(:,:,i)) / nxny
    end do

    rms = 0
    do i = 1, n - 1
      rms = rms + sum((buffer(:,:,i) - av(i) - buffer(:,:,i+1) + av(i+1))**2) / (2 * nxny)
    end do

    rms = sqrt(rms / (n - 1))
  end function

end module stacking
