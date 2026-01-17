module transform_affine_m

use globals
use transform_m
use transform_xyr_m
implicit none (type, external)
private
public :: transform_affine_t, affine_from_xyr
 !------------------------------------------------------------------------------------!

type, extends(transform_t) :: transform_affine_t
contains
   procedure :: apply  => affine_apply
   procedure :: pder   => affine_pder
   procedure :: npar   => affine_npar
end type

interface transform_affine_t
module procedure transform_affine_ctor
end interface

 !------------------------------------------------------------------------------------!

contains

 !------------------------------------------------------------------------------------!

function transform_affine_ctor(scale) result(self)
   type(transform_affine_t) :: self
   real(r64_k), intent(in) :: scale

   self%scale = scale
end function


elemental subroutine affine_apply(t, x1, y1, x2, y2)
   class(transform_affine_t), intent(in) :: t
   real(r64_k), intent(in) :: x1, y1
   real(r64_k), intent(out) :: x2, y2

   x2 = t % vec(1) + (t % vec(2) / t % scale + 1) * x1 + (t % vec(3) / t % scale) * y1
   y2 = t % vec(4) + (t % vec(5) / t % scale) * x1 + (t % vec(6) / t % scale + 1) * y1
end subroutine


pure subroutine affine_pder(t, x, y, dx, dy)
   class(transform_affine_t), intent(in) :: t
   real(r64_k), intent(in) :: x, y
   real(r64_k), dimension(:), intent(out) :: dx, dy

   dx(1) = 1
   dx(2) = x / t % scale
   dx(3) = y / t % scale
   dx(4:6) = 0

   dy(1:3) = 0
   dy(4) = 1
   dy(5) = x / t % scale
   dy(6) = y / t % scale

end subroutine

elemental function affine_npar(t) result(npar)
   class(transform_affine_t), intent(in) :: t
   integer :: npar

   npar = 6
end function

function affine_from_xyr(xyr) result(affine)
   type(transform_xyr_t), intent(in) :: xyr
   type(transform_affine_t) :: affine
   real(r64_k) :: th

   affine % scale = xyr % scale
   th = xyr % vec(3) / xyr % scale

   affine % vec(1) = xyr % vec(1)
   affine % vec(2) = (cos(th) - 1) * xyr % scale
   affine % vec(3) = -sin(th) * xyr % scale
   affine % vec(4) = xyr % vec(2)
   affine % vec(5) = sin(th) * xyr % scale
   affine % vec(6) = (cos(th) - 1) * xyr % scale
end function

end module
