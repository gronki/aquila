module findstar_op_m

use operation_m
use value_m
use str_value_m
use real_value_m
use legacy_frame_value_m
use image_frame_m
use input_args_m
use framehandling
use starlist_value_m
use findstar
use stacking, only: register_stars

implicit none (type, external)
private

type, extends(operation_t) :: findstar_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: findstar_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(findstar_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   character(len=:), allocatable :: fn
   type(starlist_value_t), allocatable :: result
   integer :: nx, ny, errno
   real(kind=img_k), allocatable :: frame
   integer :: n_stars

   call parse_number(inputs(2) % value, to_int=n_stars, err=err)
   if (check(err)) return

   allocate(result)

   select type(frame => inputs(1) % value)
   type is (legacy_frame_value_t)
      call register_stars(frame % frame % data, result % sources, limit = n_stars)
   class default
      call seterr( err, "expected image frame as `frame` parameter" )
      return
   end select

   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "findstar"
end function

pure subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) &
      argspec = [ &
      arg_entry_t(pos=1, name="frame"), &
      arg_entry_t(pos=2, name="n", default=real_value(256.0_f64)) ]

   if (present(help)) &
      help = "Finds all stars in the given image"

end subroutine

end module

