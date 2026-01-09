program test_hotpixel

  use globals
  use hotpixels
  use framehandling

  implicit none

  type(image_frame_t) :: im, im2
  logical, allocatable :: m(:,:), m2(:,:)

  call im % read_fits('dark.fits')
  allocate(m(size(im % data, 1), size(im % data, 2)))
  allocate(m2, mold = m)

  ! test: find hot

  call find_hot(im % data, 3.0_buf_k, m)

  ! test: write/read

  call write_hot(m)
  call read_hot(m2)
  print *, 'check read: ', all(m .eqv. m2)
  deallocate(m2)

  ! test: correct hotpixels

  call im2 % read_fits('veil-1.fits')
  call fix_hot(im2 % data, m)
  call im2 % write_fits('unhot.fits')

end program test_hotpixel
