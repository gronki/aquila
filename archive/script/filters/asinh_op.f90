module asinh_op_m

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

type, extends(operation_t) :: asinh_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: asinh_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(asinh_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   call val_asinh(inputs(1) % value, output, err)

end subroutine

pure function name()
   character(len=32) :: name

   name = "asinh"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) argspec = [ &
      arg_entry_t(pos=1, name="x") ]

   if (present(help)) help = "asinhes the values of the image"

end subroutine

end module


