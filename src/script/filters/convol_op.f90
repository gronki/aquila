module convol_op_m

use operation_m
use value_m
use legacy_frame_value_m
use real_value_m
use image_frame_m
use input_args_m
use convolutions

implicit none (type, external)
private

type, extends(operation_t) :: convol_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: convol_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(convol_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   type(legacy_frame_value_t), pointer :: frame, kernel
   type(legacy_frame_value_t), allocatable :: result

   call as_frame(inputs(1) % value, frame, err, "Argument frame must be an image frame")
   if (check(err)) return
   call as_frame(inputs(2) % value, kernel, err, "Argument kernel must be an image frame")
   if (check(err)) return

   result = frame
   call convol_fix(frame % frame % data, kernel % frame % data, result % frame % data, 'e')
   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "convol"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(help)) &
      help = "Convolves frame with a kernel."

   if (present(argspec)) argspec = [   &
   & arg_entry_t(pos=1, name="frame"), &
   & arg_entry_t(pos=2, name="kernel") ]

end subroutine

end module


