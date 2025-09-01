module stack_op_m

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

type, extends(operation_t) :: stack_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure, nopass :: is_elemental
   procedure :: exec_one
end type

public :: stack_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(stack_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   type(legacy_frame_value_t), allocatable :: result
   character(len=:), allocatable :: method, strategy
   integer :: n_frames, i
   real(kind=fp), allocatable :: buffer(:,:,:)
   type(image_frame_t), allocatable :: local_frames(:)


   call parse_str(inputs(2) % value, to=method, err=err)
   if (check(err)) return
   call parse_str(inputs(3) % value, to=strategy, err=err)
   if (check(err)) return

   allocate(result)

   select type(frame_seq => inputs(1) % value)
   type is (sequence_value_t)
      if (.not. allocated(frame_seq % items)) then
         call seterr (err, "stack: attempting to stack an empty sequence")
         return
      end if

      n_frames = size(frame_seq % items)
      allocate(local_frames(n_frames))

      if (n_frames == 0) then
         call seterr (err, "stack: attempting to stack an empty sequence")
         return
      end if

      do i = 1, n_frames
         select type (frame => frame_seq % items(i) % value)
         type is (legacy_frame_value_t)
         if (.not. allocated(frame % frame % data)) then
            call seterr( err, "stack: empty frame in the sequence" )
            return
        end if
        local_frames(i) = frame % frame
        class default
            call seterr( err, "stack: expected frames to be a sequence of frames" )
            return
         end select
      end do

   class default
      call seterr( err, "stack: expected frames to be a sequence of frames, got: " &
        // frame_seq % to_str() )
      return
   end select

   call collect_frames_into_buffer(local_frames, buffer)
   call stack_frames(strategy, method, local_frames, buffer, result%frame)

   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "stack"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(argspec)) &
      argspec = [ &
      arg_entry_t(pos=1, name="frames"), &
      arg_entry_t(pos=2, name="method", default=str_value("average")), &
      arg_entry_t(pos=3, name="strategy", default=str_value(""))  ]

   if (present(help)) &
      help = "Perform stacking of frames (same as in aqstack`)"

end subroutine

pure function is_elemental()
   !! return true if the operation should be performed
   !! element-wise on the sequences
   logical :: is_elemental

   is_elemental = .false.
end function is_elemental

end module


