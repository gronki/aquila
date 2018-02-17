program background

  use globals
  implicit none

  real(sp), dimension(:,:), allocatable :: im, im2
  integer :: bsize, ndim, errno = 0
  logical :: anyf
  integer :: sz(2)

  call ftdkopn(33, "in.fits", 0, bsize, errno)
  call ftrprt("stderr", errno)
  if (errno /= 0) error stop "problem opening FITS file"

  call ftgidm(33, ndim, errno)
  if (ndim /= 2) error stop "only monochrome images are supported for now"

  ! get image dimensions
  call ftgisz(33, 2, sz, errno)
  ! allocate memory and read the image
  allocate(im(sz(1), sz(2)))
  call ftgpve(33, 1, 1, product(sz), 0, im, anyf, errno)

  !----------------------------------------------------------------------------!

  main: block
    use findstar, only: shrink_mask

    logical, dimension(sz(1), sz(2)) :: msk
    real :: mean
    real(dp) :: t0, t1

    im2 = im

    call cpu_time(t0)
    call outliers(im2, 5.0, 10, msk)
    call cpu_time(t1)
    write (*,*), t1 - t0

    call shrink_mask(msk,2)
    mean = sum(im2, msk) / count(msk)

    im2(:,:) = merge(im2, mean, msk)

  end block main

  !----------------------------------------------------------------------------!

  call ftdkinit(66, "out.fits", 1, errno)
  if (errno /= 0) then
    call ftrprt("stderr", errno)
    error stop
  end if

  call ftphpr(66, .true., -32, 2, [size(im2,1), size(im2,2)], &
        & 0, 1, .true., errno)
  call ftppre(66, 1, 1, size(im2,1) * size(im2,2), im2, errno)
  call ftclos(66, errno)


end program
