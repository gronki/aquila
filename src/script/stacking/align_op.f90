module align_op_m

use operation_m
use value_m
use str_value_m
use real_value_m
use legacy_frame_value_m
use image_frame_m
use input_args_m
use framehandling
use starlist_value_m
use new_align
use findstar, only: register_stars
use transformation_value_m
use alignment_m

implicit none (type, external)
private

type, extends(operation_t) :: align_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: align_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(align_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   type(starlist_value_t), pointer :: lst, ref
   character(len=:), allocatable :: method
   type(transformation_value_t), allocatable :: result

   select type(lst_val => inputs(1) % value)
   type is (starlist_value_t)
      lst => lst_val
   class default
      call seterr(err, "starlist expected in argument nr 1 of align")
      return
   end select

   select type(ref_val => inputs(2) % value)
   type is (starlist_value_t)
      ref => ref_val
   class default
      call seterr(err, "starlist expected in argument nr 2 of align")
      return
   end select

   call parse_str(inputs(3) % value, method, err)
   if (check(err)) return

   allocate(result)
   call find_transform(lst, ref, method, result % transform, err)
   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "align"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) &
      argspec = [ &
      arg_entry_t(pos=1, name="lst"), &
      arg_entry_t(pos=2, name="ref"), &
      arg_entry_t(pos=3, name="method", default=str_value("gravity")) ]

   if (present(help)) &
      help = "Find transformation between two lists of stars"

end subroutine

subroutine find_transform(lst, ref, method, result_transform, err)
   type(starlist_value_t) :: lst, ref
   class(transform_t), allocatable :: result_transform
   type(transform_xyr_t), allocatable :: transform
   character(len=*), intent(in) :: method
   type(err_t), intent(out) :: err
   integer :: errno
   real(kind=fp) :: r0

   r0 = sum((ref%sources%x)**2 + (ref%sources%y)**2)
   r0 = sqrt(r0 / size(ref%sources))

   transform = transform_xyr_t(scale=r0)
   call classic_align(ref%sources, lst%sources, method, r0, transform, errno)

   if (errno /= 0) call seterr(err, "error in align (probably wrong method string)")

   call move_alloc(transform, result_transform)
end subroutine 

end module
