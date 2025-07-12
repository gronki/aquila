module write_op_m

use operation_m
use value_m
use str_value_m
use sequence_value_m
use real_value_m
use legacy_frame_value_m
use image_frame_m
use input_args_m
use framehandling
use starlist_value_m
use findstar
use stacking

implicit none (type, external)
private

type, extends(operation_t) :: write_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure, nopass :: is_elemental
   procedure :: exec_one
end type

public :: write_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(write_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   character(len=:), allocatable :: fn

   call parse_str(inputs(2) % value, to=fn, err=err)
   if (check(err)) return

   call inputs(1) % value % write(fn, err)
   if (check(err)) return

   allocate(output, source=inputs(1)%value)

end subroutine

pure function name()
   character(len=32) :: name

   name = "save"
end function

pure subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) &
      argspec = [ &
      arg_entry_t(pos=1, name="value"), &
      arg_entry_t(pos=2, name="fn")  ]

   if (present(help)) &
      help = "Writes the output."

end subroutine

pure function is_elemental()
   !! return true if the operation should be performed
   !! element-wise on the sequences
   logical :: is_elemental

   is_elemental = .false.
end function is_elemental

end module



