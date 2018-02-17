program test_transforms

  use transforms
  use globals
  implicit none

  type(point), dimension(24) :: p1, p2, p3
  type(affine_transform), target :: t
  class(transform), pointer :: tp
  real(dp) :: a

  t % x = -1
  t % y = -1
  a = 3.1416 / 2
  t % xx = cos(a)
  t % xy = sin(a)
  t % yx = -sin(a)
  t % yy = cos(a)

  tp => t

  p1 % x = 2
  p1 % y = 3

  call t % apply(p1 % x, p1 % y, p2 % x, p2 % y)
  call t % apply_inv(p2 % x, p2 % y, p3 % x, p3 % y)
  call tp % apply_inv(p2 % x, p2 % y, p3 % x, p3 % y)

  print *, p1
  print *, p2
  print *, p3


end program test_transforms
