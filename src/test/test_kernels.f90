program test

  use globals
  use kernels
  implicit none

  real(fp), allocatable :: k1(:,:)
  integer :: i

  allocate(k1(7,7))

  print *, shape(gausskrn_alloc(1.5_fp) * 10)
  k1 = gausskrn_alloc(3.0_fp)
  print *, shape(k1)
  k1 = gausskrn_alloc(6.0_fp)
  print *, shape(k1)

  k1 = k1 / k1(8,8)
  do i = 1,size(k1,1)
    print '(*(F8.5,1X))', k1(i,:)
  end do
  print *, shape(k1)
  call mexhakrn(size(k1,1) / 7.0_fp, k1)

  k1 = k1 / k1(8,8)

  do i = 1,size(k1,1)
    print '(*(F8.5,1X))', k1(i,:)
  end do


end program test
