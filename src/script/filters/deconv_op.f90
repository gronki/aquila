module deconv_op_m

use operation_m
use value_m
use legacy_frame_value_m
use real_value_m
use image_frame_m
use input_args_m
use deconvolutions

implicit none (type, external)
private

type, extends(operation_t) :: deconv_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: deconv_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(deconv_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   type(legacy_frame_value_t), pointer :: frame, kernel
   integer :: niter
   real(kind=fp) :: strength
   type(legacy_frame_value_t), allocatable :: result

   call as_frame(inputs(1) % value, frame, err, "Argument frame must be an image frame")
   if (check(err)) return
   call as_frame(inputs(2) % value, kernel, err, "Argument kernel must be an image frame")
   if (check(err)) return
   call as_real(inputs(3) % value, strength, err, "Argument strength must be a float")
   if (check(err)) return
   call as_int(inputs(4) % value, niter, err, "Argument niter must be an int")
   if (check(err)) return

   result = frame
   call deconvol_lr(frame % frame % data, kernel % frame % data, strength, niter, result % frame % data)
   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "deconv"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(help)) &
      help = "deconvves frame with a kernel."

   if (present(argspec)) argspec = [   &
   & arg_entry_t(pos=1, name="frame"), &
   & arg_entry_t(pos=2, name="kernel"), &
   & arg_entry_t(pos=3, name="strength", default=real_value(1.0_fp)), &
   & arg_entry_t(pos=4, name="niter", default=real_value(100._fp)) ]

end subroutine

end module


