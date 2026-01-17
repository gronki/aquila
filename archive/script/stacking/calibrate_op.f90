module calibrate_op_m

use operation_m
use value_m
use str_value_m
use real_value_m
use legacy_frame_value_m
use image_frame_m
use input_args_m
use framehandling
use starlist_value_m
use findstar, only: register_stars
use statistics

implicit none (type, external)
private

type, extends(operation_t) :: calibrate_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: calibrate_op_t

integer, parameter :: arg_frame = 1, arg_bias = 2, arg_dark = 3, arg_hot = 5, arg_flat = 4

contains

subroutine exec_one(op, inputs, output, err)
   class(calibrate_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   type(legacy_frame_value_t), pointer :: frame, bias, dark, flat
   type(legacy_frame_value_t), allocatable :: result

   call as_frame(inputs(arg_frame) % value, frame, err)
   if (check(err)) return

   allocate(result)
   result%frame = frame%frame

   if (associated(inputs(arg_bias) % value)) then
      print *, "bias subtraction enabled"
      call as_frame(inputs(arg_bias) % value, bias, err)
      if (check(err)) return
      if (any(shape(result%frame%data) /= shape(bias%frame%data))) then
         call seterr(err, "frame and bias dimensions do not match")
         return
      end if
      call fix_bias(result%frame%data, bias%frame%data)
   end if

   if (associated(inputs(arg_dark) % value)) then
      print *, "dark subtraction enabled"
      call as_frame(inputs(arg_dark) % value, dark, err)
      if (check(err)) return
      if (any(shape(result%frame%data) /= shape(dark%frame%data))) then
         call seterr(err, "frame and dark dimensions do not match")
         return
      end if
      call fix_dark(result%frame%data, dark%frame%data)
   end if

   if (associated(inputs(arg_flat) % value)) then
      print *, "flat subtraction enabled"
      call as_frame(inputs(arg_flat) % value, flat, err)
      if (check(err)) return
      if (any(shape(result%frame%data) /= shape(flat%frame%data))) then
         call seterr(err, "frame and flat dimensions do not match")
         return
      end if
      call fix_flat(result%frame%data, flat%frame%data)
   end if

   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "calibrate"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) &
      argspec = [ &
      arg_entry_t(pos=arg_frame, name="frame"), &
      arg_entry_t(pos=arg_bias, name="bias", required=.false.), &
      arg_entry_t(pos=arg_dark, name="dark", required=.false.), &
      arg_entry_t(pos=arg_flat, name="flat", required=.false.)  &
      ]

   if (present(help)) &
      help = "Do basic calibration of frames (bias, dark, flat etc.)"

end subroutine

subroutine fix_bias(light, bias)
   real(kind=fp), intent(inout) :: light(:,:)
   real(kind=fp), intent(in) :: bias(:,:)

   light(:,:) = light(:,:) - bias(:,:)
end subroutine

subroutine fix_dark(light, dark)
   real(kind=fp), intent(inout) :: light(:,:)
   real(kind=fp), intent(in) :: dark(:,:)
end subroutine

subroutine fix_flat(light, flat)
   use statistics, only: average_safe

   real(kind=fp), intent(inout) :: light(:,:)
   real(kind=fp), intent(in) :: flat(:,:)

   real(kind=fp) :: avg

   avg = average_safe(flat(::8, ::8))

   light(:,:) = light(:,:) / (flat(:,:) / avg)
end subroutine

subroutine fix_hotpixels(light, hot)
   real(kind=fp), intent(inout) :: light(:,:)
   logical, intent(in) :: hot(:,:)
end subroutine

end module