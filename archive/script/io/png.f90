module png_op_m

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
use png

implicit none (type, external)
private

type, extends(operation_t) :: png_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: png_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(png_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   character(len=:), allocatable :: fn
   integer :: ich
   real(kind=fp), allocatable :: cube(:,:,:)
   type(legacy_frame_value_t), pointer :: frame
   integer :: errno

   call parse_str(inputs(4) % value, to=fn, err=err)
   if (check(err)) return

   do ich = 1, 3
      call as_frame(inputs(ich) % value, frame, err)
      if (check(err)) return

      if (ich == 1) allocate(cube(size(frame%frame%data, 1), &
         size(frame%frame%data, 2), 3))

      cube(:, :, ich) = frame % frame % data
   end do

   call write_png(fn, cube, errno=errno)

   if (errno /= 0) then
      call seterr(err, "error writing PNG file")
      return
   end if

   output = str_value(fn)

end subroutine

pure function name()
   character(len=32) :: name

   name = "png"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) &
      argspec = [ &
      arg_entry_t(pos=1, name="r"), &
      arg_entry_t(pos=2, name="g"), &
      arg_entry_t(pos=3, name="b"), &
      arg_entry_t(pos=4, name="fn")  ]

   if (present(help)) &
      help = "Writes the png file based on one channel or 3 channels (RGB)."

end subroutine

end module




