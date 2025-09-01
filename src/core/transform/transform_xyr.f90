module transform_xyr_m

use globals
use transform_m
implicit none (type, external)
private
public :: transform_xyr_t
 !------------------------------------------------------------------------------------!

type, extends(transform_t) :: transform_xyr_t
contains
   procedure :: apply  => xyr_apply
   procedure :: pder   => xyr_pder
   procedure :: npar   => xyr_npar
end type

interface transform_xyr_t
module procedure transform_xyr_ctor
end interface

 !------------------------------------------------------------------------------------!

contains

 !------------------------------------------------------------------------------------!

function transform_xyr_ctor(scale) result(self)
   type(transform_xyr_t) :: self
   real(fp), intent(in), optional :: scale

   if (present(scale)) self%scale = scale
end function


elemental subroutine xyr_apply(t, x1, y1, x2, y2)
   class(transform_xyr_t), intent(in) :: t
   real(fp), intent(in) :: x1, y1
   real(fp), intent(out) :: x2, y2
   real(fp) :: th

   th = t % vec(3) / t % scale
   x2 = t % vec(1) + cos(th) * x1 - sin(th) * y1
   y2 = t % vec(2) + sin(th) * x1 + cos(th) * y1
end subroutine


pure subroutine xyr_pder(t, x, y, dx, dy)
   class(transform_xyr_t), intent(in) :: t
   real(fp), intent(in) :: x, y
   real(fp), dimension(:), intent(out) :: dx, dy
   real(fp) :: th

   dx(1) = 1
   dy(1) = 0
   dx(2) = 0
   dy(2) = 1

   th = t % vec(3) / t % scale
   dx(3) = (- sin(th) * x - cos(th) * y) / (t % scale)
   dy(3) = (  cos(th) * x - sin(th) * y) / (t % scale)
end subroutine

elemental function xyr_npar(t) result(npar)
   class(transform_xyr_t), intent(in) :: t
   integer :: npar

   npar = 3
end function

end module
