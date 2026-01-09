module aquila_c_binding

use iso_c_binding
use globals
implicit none

type, bind(c) :: buffer_descriptor_t
   type(c_ptr) :: data
   integer(i64_k) :: rows, cols
end type

contains

function from_descriptor(descr)
   type(buffer_descriptor_t), intent(in) :: descr
   real(buf_k), pointer, contiguous :: from_descriptor(:,:)
   integer(i64_k) :: arrshape(2)

   arrshape(1) = descr%rows
   arrshape(2) = descr%cols

   call c_f_pointer(descr%data, from_descriptor, arrshape)
end function

end module
