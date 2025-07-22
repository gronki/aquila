module project_op_m

use operation_m
use value_m
use str_value_m
use real_value_m
use legacy_frame_value_m
use image_frame_m
use input_args_m
use framehandling
use starlist_value_m
use stacking, only: register_stars
use transformation_value_m
use projection_m
use legacy_frame_value_m

implicit none (type, external)
private

type, extends(operation_t) :: project_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: project_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(project_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   type(legacy_frame_value_t), pointer :: frame
   type(legacy_frame_value_t), allocatable :: result
   integer :: ni, nj
   real(fp) :: resample

   call as_frame(inputs(1) % value, frame, err)
   if (check(err)) return

   call parse_number(inputs(3) % value, to_real=resample, err=err)
   if (check(err)) return

   ni = size(frame%frame%data, 1)
   nj = size(frame%frame%data, 2)

   allocate(result)
   allocate(result%frame%data(ceiling(ni * resample), ceiling(nj * resample)))

   select type(tx => inputs(2) % value)
   class is (transformation_value_t)
      if (resample /= 1.0_fp) then
      call project_bilinear(tx%transform, frame%frame%data, result%frame%data, resample=resample)
      else
      call project_bilinear(tx%transform, frame%frame%data, result%frame%data)
    end if
   class default
      call seterr(err, "transformation expected in argument nr 2 of project")
      return
   end select

   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "project"
end function

pure subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) &
      argspec = [ &
      arg_entry_t(pos=1, name="frame"), &
      arg_entry_t(pos=2, name="tx"), &
      arg_entry_t(pos=3, name="resample", default=real_value(1.0_fp)) ]

   if (present(help)) &
      help = "Given a transformation, project the image buffer."

end subroutine


end module
