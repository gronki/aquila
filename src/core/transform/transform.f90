module transform_m

use globals
implicit none
private
public :: transform_t

integer, parameter, public :: TRANSFORM_MAX_PAR = 16

type, abstract :: transform_t
   real(r64_k) :: scale = 1
   real(r64_k) :: vec(TRANSFORM_MAX_PAR) = 0
contains
   procedure(iface_npar), deferred :: npar
   procedure(iface_apply), deferred :: apply
   procedure(iface_pder), deferred :: pder
end type

abstract interface

elemental function iface_npar(t) result(npar)
   import transform_t
   class(transform_t), intent(in) :: t
   integer :: npar
end function

elemental subroutine iface_apply(t, x1, y1, x2, y2)
   import transform_t, r64_k
   class(transform_t), intent(in) :: t
   real(r64_k), intent(in) :: x1, y1
   real(r64_k), intent(out) :: x2, y2
end subroutine

pure subroutine iface_pder(t, x, y, dx, dy)
   import transform_t, r64_k
   class(transform_t), intent(in) :: t
   real(r64_k), intent(in) :: x, y
   real(r64_k), dimension(:), intent(out) :: dx, dy
end subroutine

end interface

end module
