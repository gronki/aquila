module stretch_op_m

use operation_m
use value_m
use legacy_frame_value_m
use real_value_m
use image_frame_m
use input_args_m
use numeric_arithmetic_m
use statistics

implicit none (type, external)
private

type, extends(operation_t) :: stretch_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: stretch_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(stretch_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   type(legacy_frame_value_t), pointer :: frame
   type(legacy_frame_value_t), allocatable :: result
   real(fp) :: low_value, high_value

   call as_frame(inputs(1) % value, frame, err, &
      errmsg="First argument to stretch must be a frame.")
   if (check(err)) return
   call parse_number(inputs(2) % value, to_real=low_value, err=err, &
      errmsg="argument lo must have a numeric value.")
   if (check(err)) return
   call parse_number(inputs(3) % value, to_real=high_value, err=err, &
      errmsg="argument hi must have a numeric value.")
   if (check(err)) return

   allocate(result)
   result % frame = strip_buffer(frame % frame)
   result % frame % data = (frame % frame % data - low_value) / (high_value - low_value)

   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "stretch"
end function

pure subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) argspec = [ &
      arg_entry_t(pos=1, name="frame"), &
      arg_entry_t(pos=2, name="lo"), &
      arg_entry_t(pos=3, name="hi")  &
      ]

   if (present(help)) help = "Stretches the values of the image&
   & relative to the standard deviation, so that the output range&
   & is between 0 and 1."

end subroutine

end module


