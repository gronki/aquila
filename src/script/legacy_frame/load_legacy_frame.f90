module load_legacy_frame_op_m

use operation_m
use value_m
use str_value_m
use legacy_frame_value_m
use input_args_m

implicit none (type, external)
private

type, extends(operation_t) :: load_legacy_frame_op_t
contains
   procedure, nopass :: name
   procedure, nopass :: get_info
   procedure :: exec_one
end type

public :: load_legacy_frame_op_t

contains

subroutine exec_one(op, inputs, output, err)
    class(load_legacy_frame_op_t), intent(in) :: op !! operation
    type(value_ref_t), intent(in) :: inputs(:) !! operation inputs
    class(value_t), intent(out), allocatable :: output !! output/result
    type(err_t), intent(inout) :: err !! error
    real(kind=f64) :: x, exponent
    character(len=:), allocatable :: fn
 
    call parse_str(inputs(1) % value, to=fn, err=err)
    if (check(err)) return
 
    
 
 end subroutine
 
 pure function name() 
    character(len=32) :: name
 
    name = "load_frame"
 end function
 
 pure subroutine get_info(argspec, help)
    type(arg_entry_t), intent(out), allocatable, optional :: argspec(:)
    character(len=:), intent(out), allocatable, optional :: help
 
    if (present(argspec)) &
       argspec = [ arg_entry_t(pos=1, name="file") ]
 
    if (present(help)) &
       help = "Loads FITS file into legacy frame object."
 
 end subroutine

end module
