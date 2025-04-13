program test_deconvolution

  use convolutions
  use deconvolutions
  use globals
  use kernels
  use framehandling

  implicit none

  type(frame_t) :: img1, img2
  real(fp), dimension(:,:), allocatable :: k
  real(fp), parameter :: ffz = 0.33

  call img1 % read_fits('deconv.fits')
  call img2 % check_shape(size(img1 % data, 1), size(img1 % data, 2))
  img2 % data (:,:) = 0

  deconvol_test: block
    character(len = 256) :: fn
    integer :: i, niter
    real(fp) :: w
    k = gausskrn_alloc(1.5_fp)
    do i = 1, 11
      niter = nint(1 * (128 / 1.0)**((i - 1) / 10.0))
      print *, i, niter
      call deconvol_lr(img1 % data, k, 1.0_fp, niter, img2 % data)
      w = ffz * (1 + log(24.0)) / (1 + log(real(niter)))
      img2 % data(:,:) = img1 % data * (1 - w) + img2 % data * w
      write (fn, '("deconvz", i0.2, ".fits")') i
      call img2 % write_fits(trim(fn))
    end do
  end block deconvol_test

  deconvol_test_2: block
    character(len = 256) :: fn
    real(fp) :: ksize
    integer :: i
    do i = 1, 11
      ksize = 0.6 * (2.5 / 0.6)**((i - 1) / 10.0)
      print *, i, ksize
      k = gausskrn_alloc(ksize)
      call deconvol_lr(img1 % data, k, 1.0_fp, 32, img2 % data)
      img2 % data(:,:) = img1 % data * (1 - ffz) + img2 % data * ffz
      write (fn, '("deconvq", i0.2, ".fits")') i
      call img2 % write_fits(trim(fn))
    end do
  end block deconvol_test_2

end program test_deconvolution
