program test_deconvolution

  use convolutions
  use deconvolutions
  use globals
  use kernels
  use framehandling

  implicit none

  type(frame_t) :: img1, img2, imkr
  real(fp), dimension(:,:), allocatable :: k
  real(fp), parameter :: ff0 = 1.7, ffz = 0.3

  call img1 % read_fits('deconv1.fits')
  call img2 % alloc_zeros(img1)

  k = gausskrn_alloc(ff0)
  call deconvol_lr(img1 % data, k, 1024, img2 % data)
  img2 % data(:,:) = img1 % data * (1 - ffz) + img2 % data * ffz
  call img2 % write_fits('deconvz.fits')

  k = mexhakrn_alloc(ff0)
  ! k = reshape([0, -1, 0, -1, 4, -1, 0, -1, 0], [3,3]) / 4.0_fp
  call convol_fix(img1 % data, k, img2 % data, 'e')
  img2 % data(:,:) = img1 % data * (1 - ffz) + img2 % data * ffz
  call img2 % write_fits('deconvu.fits')

  call imkr % read_fits('deconvk.fits')
  imkr % data = imkr % data / sum(imkr % data)

  call convol_fix(img1 % data, imkr % data, img2 % data, 'r')
  call img2 % write_fits('deconv2.fits')

  call deconvol_lr(img2 % data, imkr % data, 1024, img1 % data)
  call img1 % write_fits('deconv3.fits')

end program test_deconvolution
