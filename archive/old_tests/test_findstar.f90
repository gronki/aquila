program test_findstar

  use globals
  implicit none

  real(buf_k), allocatable :: im(:,:), im2(:,:,:)
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

    use findstar
    use kernels, only: mexhakrn_alloc
    use convolutions, only: convol_fix

    type(source_t), allocatable :: list(:)
    real(buf_k), allocatable :: imcv(:,:)
    real(buf_k), dimension(5,5) :: krn = &
                 reshape([  0, -1, -2, -1,  0, &
                           -1,  0,  2,  0, -1, &
                           -2,  2,  8,  2, -2, &
                           -1,  0,  2,  0, -1, &
                            0, -1, -2, -1,  0  ], [5,5]) / 64.0_buf_k
    integer :: nstars, i

    allocate(im2(size(im,1), size(im,2), 2), imcv(size(im,1), size(im,2)))
    call convol_fix(im, krn, imcv, 'r')
    ! where (im2 < 0) im2 = -tiny(im2)

    call aqfindstar(imcv, list, limit = 250)

    im2(:,:,1) = im
    im2(:,:,2) = imcv

    do i = 1, size(list)
      write (11, *) 'circle', list(i) % x, list(i) % y, 3.75 * list(i) % rms
      write (12, '(2F12.2,G12.4,F12.3,2F12.4)')  list(i) % x, list(i) % y, list(i) % flux, list(i) % rms, list(i) % deviation_xy, list(i) % deviation_uv
    end do

  end block main

  !----------------------------------------------------------------------------!

  call ftdkinit(66, "out.fits", 1, errno)
  if (errno /= 0) then
    call ftrprt("stderr", errno)
    error stop
  end if

  call ftphpr(66, .true., -32, rank(im2), shape(im2), 0, 1, .true., errno)
  call ftppre(66, 1, 1, size(im2), im2, errno)
  call ftclos(66, errno)

end program
