program test

  use globals
  use kernels
  implicit none

  real, allocatable :: k1(:,:)
  integer :: i

  allocate(k1(7,7))

  print *, shape(gausskrn_alloc(1.5) * 10)
  k1 = gausskrn_alloc(3.0)
  print *, shape(k1)
  k1 = gausskrn_alloc(6.0)
  print *, shape(k1)

  k1 = k1 / k1(8,8)
  do i = 1,size(k1,1)
    print '(*(F8.5,1X))', k1(i,:)
  end do
  print *, shape(k1)
  call mexhakrn(size(k1,1) / 7.0, k1)

  k1 = k1 / k1(8,8)

  do i = 1,size(k1,1)
    print '(*(F8.5,1X))', k1(i,:)
  end do


end program test
