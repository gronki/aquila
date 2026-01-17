module numeric_arithmetic_m

use value_m
use real_value_m
use legacy_frame_value_m
use image_frame_m, only: strip_buffer
use error_m

implicit none (type, external)
private

interface operator(+)
module procedure add__real__frame
module procedure add__frame__real
module procedure add__frame__frame
module procedure add__real__real
end interface

public :: operator(+)

interface operator(-)
module procedure sub__real__frame
module procedure sub__frame__real
module procedure sub__frame__frame
module procedure sub__real__real
end interface

public :: operator(-)

interface operator(*)
module procedure mul__real__frame
module procedure mul__frame__real
module procedure mul__frame__frame
module procedure mul__real__real
end interface

public :: operator(*)

interface operator(/)
module procedure div__real__frame
module procedure div__frame__real
module procedure div__frame__frame
module procedure div__real__real
end interface

public :: operator(/)

interface asinh
module procedure asinh__real
module procedure asinh__frame
end interface

public :: asinh

public :: val_add, val_sub, val_mul, val_div, val_asinh

contains

! --------------------- + ADD ------------------------

elemental function add__real__frame(v1, v2) result(v)
   type(real_value_t), intent(in) :: v1
   type(legacy_frame_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v2%frame)
   v%frame%data = v1%value + v2%frame%data
end function

elemental function add__frame__real(v1, v2) result(v)
   type(legacy_frame_value_t), intent(in) :: v1
   type(real_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v1%frame)
   v%frame%data = v1%frame%data + v2%value
end function

elemental function add__frame__frame(v1, v2) result(v)
   type(legacy_frame_value_t), intent(in) :: v1
   type(legacy_frame_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v1%frame)
   v%frame%data = v1%frame%data + v2%frame%data
   v%frame%exptime = v1%frame%exptime + v2%frame%exptime
end function

elemental function add__real__real(v1, v2) result(v)
   type(real_value_t), intent(in) :: v1
   type(real_value_t), intent(in) :: v2
   type(real_value_t) :: v

   v%value = v1%value + v2%value
end function

! --------------------- - SUB ------------------------

elemental function sub__real__frame(v1, v2) result(v)
   type(real_value_t), intent(in) :: v1
   type(legacy_frame_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v2%frame)
   v%frame%data = v1%value - v2%frame%data
end function

elemental function sub__frame__real(v1, v2) result(v)
   type(legacy_frame_value_t), intent(in) :: v1
   type(real_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v1%frame)
   v%frame%data = v1%frame%data - v2%value
end function

elemental function sub__frame__frame(v1, v2) result(v)
   type(legacy_frame_value_t), intent(in) :: v1
   type(legacy_frame_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v1%frame)
   v%frame%data = v1%frame%data - v2%frame%data
end function

elemental function sub__real__real(v1, v2) result(v)
   type(real_value_t), intent(in) :: v1
   type(real_value_t), intent(in) :: v2
   type(real_value_t) :: v

   v%value = v1%value - v2%value
end function

 ! --------------------- * MUL ------------------------

elemental function mul__real__frame(v1, v2) result(v)
   type(real_value_t), intent(in) :: v1
   type(legacy_frame_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v2%frame)
   v%frame%data = v1%value * v2%frame%data
   v%frame%exptime = v1%value * v2%frame%exptime
end function

elemental function mul__frame__real(v1, v2) result(v)
   type(legacy_frame_value_t), intent(in) :: v1
   type(real_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v1%frame)
   v%frame%data = v1%frame%data * v2%value
   v%frame%exptime = v1%frame%exptime * v2%value
end function

elemental function mul__frame__frame(v1, v2) result(v)
   type(legacy_frame_value_t), intent(in) :: v1
   type(legacy_frame_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v1%frame)
   v%frame%data = v1%frame%data * v2%frame%data
end function

elemental function mul__real__real(v1, v2) result(v)
   type(real_value_t), intent(in) :: v1
   type(real_value_t), intent(in) :: v2
   type(real_value_t) :: v

   v%value = v1%value * v2%value
end function

! --------------------- / DIV ------------------------

elemental function div__real__frame(v1, v2) result(v)
   type(real_value_t), intent(in) :: v1
   type(legacy_frame_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v2%frame)
   v%frame%data = v1%value / v2%frame%data
end function

elemental function div__frame__real(v1, v2) result(v)
   type(legacy_frame_value_t), intent(in) :: v1
   type(real_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v1%frame)
   v%frame%data = v1%frame%data / v2%value
   v%frame%exptime = v1%frame%exptime / v2%value
end function

elemental function div__frame__frame(v1, v2) result(v)
   type(legacy_frame_value_t), intent(in) :: v1
   type(legacy_frame_value_t), intent(in) :: v2
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v1%frame)
   v%frame%data = v1%frame%data / v2%frame%data
end function

elemental function div__real__real(v1, v2) result(v)
   type(real_value_t), intent(in) :: v1
   type(real_value_t), intent(in) :: v2
   type(real_value_t) :: v

   v%value = v1%value / v2%value
end function

! --------------------- ASINH ------------------------

elemental function asinh__frame(v1) result(v)
   type(legacy_frame_value_t), intent(in) :: v1
   type(legacy_frame_value_t) :: v

   v%frame = strip_buffer(v1%frame)
   v%frame%data = asinh(v1%frame%data)
end function

elemental function asinh__real(v1) result(v)
   type(real_value_t), intent(in) :: v1
   type(real_value_t) :: v

   v%value = asinh(v1 % value)
end function

! --------------------- GENERAL ------------------------

subroutine val_add(v1, v2, v, err)
   class(value_t), intent(in) :: v1
   class(value_t), intent(in) :: v2
   class(value_t), intent(inout), allocatable :: v
   type(err_t), intent(out), optional :: err

   select type(v1)
   type is(real_value_t)
      select type(v2)
      type is(real_value_t)
         v = add__real__real(v1, v2)
      type is(legacy_frame_value_t)
         v = add__real__frame(v1, v2)
      class default
         call seterr(err, "arithmetic not allowed on " // v2%to_str())
      end select
   type is(legacy_frame_value_t)
      select type(v2)
      type is(real_value_t)
         v = add__frame__real(v1, v2)
      type is(legacy_frame_value_t)
         if (any(shape(v1%frame%data) /= shape(v2%frame%data))) then
            call seterr(err, "operands must have conforming shape, but got " // &
               v1%to_str() // " and " // v2%to_str()  )
            return
         end if
         v = add__frame__frame(v1, v2)
      class default
         call seterr(err, "arithmetic not allowed on " // v2%to_str())
      end select
   class default
      call seterr(err, "arithmetic not allowed on " // v1%to_str())
   end select
end subroutine

subroutine val_sub(v1, v2, v, err)
   class(value_t), intent(in) :: v1
   class(value_t), intent(in) :: v2
   class(value_t), intent(inout), allocatable :: v
   type(err_t), intent(out), optional :: err

   select type(v1)
   type is(real_value_t)
      select type(v2)
      type is(real_value_t)
         v = sub__real__real(v1, v2)
      type is(legacy_frame_value_t)
         v = sub__real__frame(v1, v2)
      class default
         call seterr(err, "arithmetic not allowed on " // v2%to_str())
      end select
   type is(legacy_frame_value_t)
      select type(v2)
      type is(real_value_t)
         v = sub__frame__real(v1, v2)
      type is(legacy_frame_value_t)
         if (any(shape(v1%frame%data) /= shape(v2%frame%data))) then
            call seterr(err, "operands must have conforming shape, but got " // &
               v1%to_str() // " and " // v2%to_str()  )
            return
         end if
         v = sub__frame__frame(v1, v2)
      class default
         call seterr(err, "arithmetic not allowed on " // v2%to_str())
      end select
   class default
      call seterr(err, "arithmetic not allowed on " // v1%to_str())
   end select
end subroutine

subroutine val_mul(v1, v2, v, err)
   class(value_t), intent(in) :: v1
   class(value_t), intent(in) :: v2
   class(value_t), intent(inout), allocatable :: v
   type(err_t), intent(out), optional :: err

   select type(v1)
   type is(real_value_t)
      select type(v2)
      type is(real_value_t)
         v = mul__real__real(v1, v2)
      type is(legacy_frame_value_t)
         v = mul__real__frame(v1, v2)
      class default
         call seterr(err, "arithmetic not allowed on " // v2%to_str())
      end select
   type is(legacy_frame_value_t)
      select type(v2)
      type is(real_value_t)
         v = mul__frame__real(v1, v2)
      type is(legacy_frame_value_t)
         if (any(shape(v1%frame%data) /= shape(v2%frame%data))) then
            call seterr(err, "operands must have conforming shape, but got " // &
               v1%to_str() // " and " // v2%to_str()  )
            return
         end if
         v = mul__frame__frame(v1, v2)
      class default
         call seterr(err, "arithmetic not allowed on " // v2%to_str())
      end select
   class default
      call seterr(err, "arithmetic not allowed on " // v1%to_str())
   end select
end subroutine

subroutine val_div(v1, v2, v, err)
   class(value_t), intent(in) :: v1
   class(value_t), intent(in) :: v2
   class(value_t), intent(inout), allocatable :: v
   type(err_t), intent(out), optional :: err

   select type(v1)
   type is(real_value_t)
      select type(v2)
      type is(real_value_t)
         v = div__real__real(v1, v2)
      type is(legacy_frame_value_t)
         v = div__real__frame(v1, v2)
      class default
         call seterr(err, "arithmetic not allowed on " // v2%to_str())
      end select
   type is(legacy_frame_value_t)
      select type(v2)
      type is(real_value_t)
         v = div__frame__real(v1, v2)
      type is(legacy_frame_value_t)
         if (any(shape(v1%frame%data) /= shape(v2%frame%data))) then
            call seterr(err, "operands must have conforming shape, but got " // &
               v1%to_str() // " and " // v2%to_str()  )
            return
         end if
         v = div__frame__frame(v1, v2)
      class default
         call seterr(err, "arithmetic not allowed on " // v2%to_str())
      end select
   class default
      call seterr(err, "arithmetic not allowed on " // v1%to_str())
   end select
end subroutine

subroutine val_asinh(v1, v, err)
   class(value_t), intent(in) :: v1
   class(value_t), intent(inout), allocatable :: v
   type(err_t), intent(out), optional :: err

   select type(v1)
   type is(real_value_t)
      v = asinh__real(v1)
   type is(legacy_frame_value_t)
      v = asinh__frame(v1)
   class default
      call seterr(err, "arithmetic not allowed on " // v1%to_str())
   end select

end subroutine

end module
