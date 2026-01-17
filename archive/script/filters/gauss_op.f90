module kernel_op_m

use operation_m
use value_m
use legacy_frame_value_m
use real_value_m
use image_frame_m
use input_args_m
use kernels
use str_value_m

implicit none (type, external)
private

type, extends(operation_t) :: kernel_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: kernel_op_t

contains

subroutine exec_one(op, inputs, output, err)
   class(kernel_op_t), intent(in) :: op !! operation
   type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
   class(value_t), intent(out), allocatable :: output !! output/result
   type(err_t), intent(inout) :: err !! error

   real(kind=fp) :: fwhm
   type(legacy_frame_value_t), allocatable :: result
   character(len=:), allocatable :: type

   call as_real(inputs(1) % value, fwhm, err, "Argument fwhm must be a number.")
   if (check(err)) return
   call parse_str(inputs(2) % value, type, err, "Argument type must be a string: gauss or mexhat.")
   if (check(err)) return

   allocate(result)
   select case (type)
   case ("gauss")
      result % frame % data = gausskrn_alloc(fwhm)
   case ("mexhat")
      result % frame % data = mexhakrn_alloc(fwhm)
   case default
      call seterr(err, "type must be: kernel or mexhat")
      return
   end select

   call move_alloc(result, output)

end subroutine

pure function name()
   character(len=32) :: name

   name = "kernel"
end function

subroutine get_info(argspec, help)
   type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
   character(len=:), intent(out), allocatable, optional :: help

   if (present(help)) &
      help = "Returns a kernelian kernel with given fwhm."

   if (present(argspec)) argspec = [  &
   & arg_entry_t(pos=1, name="fwhm"), &
   & arg_entry_t(pos=2, name="type", default=str_value("gauss")) ]

end subroutine

end module


