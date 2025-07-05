module transform_m

use globals
implicit none
private
public :: transform_t

type, abstract :: transform_t
   real(fp) :: scale = 1
   real(fp), allocatable :: vec(:)
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
   import transform_t, fp
   class(transform_t), intent(in) :: t
   real(fp), intent(in) :: x1, y1
   real(fp), intent(out) :: x2, y2
end subroutine

pure subroutine iface_pder(t, x, y, dx, dy)
   import transform_t, fp
   class(transform_t), intent(in) :: t
   real(fp), intent(in) :: x, y
   real(fp), dimension(:), intent(out) :: dx, dy
end subroutine

end interface

end module
