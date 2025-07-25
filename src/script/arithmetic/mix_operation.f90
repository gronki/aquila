module mix_op_m

use operation_m
use value_m
use legacy_frame_value_m
use real_value_m
use image_frame_m
use input_args_m
use numeric_arithmetic_m

implicit none (type, external)
private

type, extends(operation_t) :: mix_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: mix_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(mix_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   integer :: ipar
   class(value_t), allocatable :: product, tmp

   if (modulo(size(inputs), 2) /= 0) then
      call seterr(err, "The number of parameters for MIX must be even.")
      return
   end if

   output = real_value(0.0_f64)

   do ipar = 1, size(inputs), 2
      call val_mul(inputs(ipar) % value, inputs(ipar+1) % value, product)
      call val_add(output, product, tmp)
      call move_alloc(tmp, output)
   end do

end subroutine

pure function name()
   character(len=32) :: name

   name = "mix"
end function

pure subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(help)) &
      help = "Mixes frames and coefficinets in a dot-product convention.&
   & For example, mix(a, b, c, d) = a*b + c*d. The number of parameters&
   & must be an even number."

end subroutine

end module

