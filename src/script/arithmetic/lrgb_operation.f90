module lrgb_op_m

use operation_m
use value_m
use legacy_frame_value_m
use real_value_m
use image_frame_m
use input_args_m
use numeric_arithmetic_m
use sequence_value_m

implicit none (type, external)
private

type, extends(operation_t) :: lrgb_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: lrgb_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(lrgb_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   integer :: ipar
   class(sequence_value_t), allocatable :: result
   class(value_t), allocatable :: chroma_sum, tmp

   if (size(inputs) < 2) then
      call seterr(err, "The number of parameters for LRGB must be at least 2.")
      return
   end if

   allocate(result)
   allocate(result%items(size(inputs) - 1))

   chroma_sum = real_value(0.0_f64)

   do ipar = 2, size(inputs)
      call val_add(chroma_sum, inputs(ipar) % value, tmp, err)
      if (check(err)) return
      call move_alloc(tmp, chroma_sum)
   end do

   call val_div(inputs(1) % value, chroma_sum, tmp, err)
   if (check(err)) return

   do ipar = 2, size(inputs)
      call val_mul(tmp, inputs(ipar) % value, result%items(ipar-1)%value, err)
      if (check(err)) return
   end do

   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "lrgb"
end function

pure subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(help)) &
      help = "lrgb(l, a, b, c, ...) yields sequence {f * a, f * b, ...}&
      & where f = l / (a + b + ...)."

end subroutine

end module

