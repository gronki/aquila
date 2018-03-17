module transforms

  use globals
  implicit none

  type :: point
    real(dp) :: x, y
  end type

  !----------------------------------------------------------------------------!

  type, abstract :: transform
  contains
    procedure(transform_det),        deferred :: det
    procedure(transform_apply),      deferred :: apply
    procedure(transform_apply_inv),  deferred :: apply_inv
    procedure(transform_get_components), deferred :: components
    procedure(transform_get_pder), deferred :: pder
  end type

  abstract interface
    elemental function transform_det(t) result(det)
      import transform, dp
      class(transform), intent(in) :: t
      real(dp) :: det
    end function
    subroutine transform_get_components(t,c)
      import transform, dp
      class(transform), intent(in) :: t
      real(dp), intent(out) :: c(:)
    end subroutine
    subroutine transform_get_pder(t,x,y,dx,dy)
      import transform, dp
      class(transform), intent(in) :: t
      real(dp), intent(in) :: x,y
      real(dp), dimension(:), intent(out) :: dx,dy
    end subroutine
    elemental subroutine transform_apply(t, x1, y1, x2, y2)
      import transform, dp
      class(transform), intent(in) :: t
      real(dp), intent(in)  :: x1, y1
      real(dp), intent(out) :: x2, y2
    end subroutine
    elemental subroutine transform_apply_inv(t, x1, y1, x2, y2)
      import transform, dp
      class(transform), intent(in) :: t
      real(dp), intent(in)  :: x1, y1
      real(dp), intent(out) :: x2, y2
    end subroutine
  end interface

  !----------------------------------------------------------------------------!

  type, extends(transform) :: affine_transform
    real(dp) :: x = 0, xx = 1, xy = 0
    real(dp) :: y = 0, yx = 0, yy = 1
  contains
    procedure :: apply => affine_apply
    procedure :: apply_inv => affine_apply_inv
    procedure :: det => affine_det
    procedure :: components => affine_get_components
    procedure :: pder => affine_get_pder
  end type

  !----------------------------------------------------------------------------!

  type, extends(transform) :: iso_transform
    real(dp) :: x = 0, y = 0, r = 0
  contains
    procedure :: apply => iso_apply
    procedure :: apply_inv => iso_apply_inv
    procedure :: det => iso_det
    procedure :: components => iso_get_components
    procedure :: pder => iso_get_pder
  end type

  !----------------------------------------------------------------------------!

contains

  !----------------------------------------------------------------------------!

  elemental subroutine affine_apply(t, x1, y1, x2, y2)
    class(affine_transform), intent(in) :: t
    real(dp), intent(in)  :: x1, y1
    real(dp), intent(out) :: x2, y2
    x2 = (t % x) + (t % xx) * x1 + (t % xy) * y1
    y2 = (t % y) + (t % yx) * x1 + (t % yy) * y1
  end subroutine

  elemental subroutine affine_apply_inv(t, x1, y1, x2, y2)
    class(affine_transform), intent(in) :: t
    real(dp), intent(in)  :: x1, y1
    real(dp), intent(out) :: x2, y2
    x2 = (   (t % yy) * (x1 - (t % x)) &
    &      - (t % xy) * (y1 - (t % y)) ) / (t % det())
    y2 = ( - (t % yx) * (x1 - (t % x)) &
    &      + (t % xx) * (y1 - (t % y)) ) / (t % det())
  end subroutine

  elemental function affine_det(t) result(det)
    class(affine_transform), intent(in) :: t
    real(dp) :: det
    det = (t % xx) * (t % yy) - (t % xy) * (t % yx)
  end function

  subroutine affine_get_components(t,c)
    class(affine_transform), intent(in) :: t
    real(dp), intent(out) :: c(:)
    C(1) = T % X
    C(2) = T % XX
    C(3) = T % XY
    C(4) = T % Y
    C(5) = T % YX
    C(6) = T % YY
  end subroutine

  subroutine affine_get_pder(t,x,y,dx,dy)
    class(affine_transform), intent(in) :: t
    real(dp), intent(in) :: x,y
    real(dp), dimension(:), intent(out) :: dx,dy
    dx(1) = 1
    dx(2) = x
    dx(3) = y
    dx(4:6) = 0
    dy(1:3) = 0
    dy(4) = 1
    dy(5) = x
    dy(6) = y
  end subroutine

  !----------------------------------------------------------------------------!


  elemental subroutine iso_apply(t, x1, y1, x2, y2)
    class(iso_transform), intent(in) :: t
    real(dp), intent(in)  :: x1, y1
    real(dp), intent(out) :: x2, y2
    x2 = (t % x) + cos(t % r) * x1 - sin(t % r) * y1
    y2 = (t % y) + sin(t % r) * x1 + cos(t % r) * y1
  end subroutine

  elemental subroutine iso_apply_inv(t, x1, y1, x2, y2)
    class(iso_transform), intent(in) :: t
    real(dp), intent(in)  :: x1, y1
    real(dp), intent(out) :: x2, y2
    x2 = (   cos(t % r) * (x1 - (t % x)) &
    &      + sin(t % r) * (y1 - (t % y)) )
    y2 = ( - sin(t % r) * (x1 - (t % x)) &
    &      + cos(t % r) * (y1 - (t % y)) )
  end subroutine

  elemental function iso_det(t) result(det)
    class(iso_transform), intent(in) :: t
    real(dp) :: det
    det = 1
  end function

  subroutine iso_get_components(t,c)
    class(iso_transform), intent(in) :: t
    real(dp), intent(out) :: c(:)
    C(1) = T % X
    C(2) = T % Y
    C(3) = T % R
  end subroutine

  subroutine iso_get_pder(t,x,y,dx,dy)
    class(iso_transform), intent(in) :: t
    real(dp), intent(in) :: x,y
    real(dp), dimension(:), intent(out) :: dx,dy
    dx(1) = 1
    dy(1) = 0
    dx(2) = 0
    dy(2) = 1
    dx(3) = - sin(t % r) * x - cos(t % r) * y
    dy(3) =   cos(t % r) * x - sin(t % r) * y
  end subroutine

  !----------------------------------------------------------------------------!

  subroutine project(t, dest, src)
    class(transform) :: t
    real(dp), dimension(:,:), intent(in)  :: src
    real(dp), dimension(:,:), intent(out) :: dest
    integer   :: id, jd
    integer   :: ik, jk
    real(dp)  :: is, js

    do jd = 1, size(dest,2)
      do id = 1, size(dest,1)
        call t % apply(real(id,dp), real(jd,dp), is, js)

        ik = max(1, min(floor(is), size(src,1) - 1))
        jk = max(1, min(floor(js), size(src,2) - 1))

        if ( abs(2 * (is - ik) - 1) <= 2 &
              .and. abs(2 * (js - jk) - 1) <= 2 ) then
          dest(id,jd) = src(ik,     jk    ) * (1 - is + ik) * (1 - js + jk)  &
          &           + src(ik + 1, jk    ) * (    is - ik) * (1 - js + jk)  &
          &           + src(ik,     jk + 1) * (1 - is + ik) * (    js - jk)  &
          &           + src(ik + 1, jk + 1) * (    is - ik) * (    js - jk)
        end if
      end do
    end do

  end subroutine

end module
