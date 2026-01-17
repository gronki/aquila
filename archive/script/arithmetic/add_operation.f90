module add_op_m

use operation_m
use value_m
use legacy_frame_value_m
use real_value_m
use image_frame_m
use input_args_m
use numeric_arithmetic_m

implicit none (type, external)
private

type, extends(operation_t) :: add_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: add_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(add_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   integer :: ipar
   class(value_t), allocatable :: tmp

   output = real_value(0.0_f64)

   do ipar = 1, size(inputs)
      call val_add(output, inputs(ipar) % value, tmp, err)
      if (check(err)) return
      call move_alloc(tmp, output)
   end do

end subroutine

pure function name()
   character(len=32) :: name

   name = "add"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(help)) help = "Adds all frame pixels and scalars&
   & listed as arguments."

end subroutine

end module

