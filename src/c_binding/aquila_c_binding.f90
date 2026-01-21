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

subroutine c_f_string(c_str, f_str)
   character(kind=c_char, len=1), intent(in) :: c_str(*)
   character(kind=c_char, len=*), intent(out) :: f_str
   integer :: i, str_len

   str_len = len(f_str)

   i = 1
   do while (ichar(c_str(i)) /= 0 .and. i <= str_len)
      i = i + 1
   end do

   f_str = transfer(c_str(1:i-1), f_str)
   if (i <= str_len) f_str(i:str_len) = ""

end subroutine

end module
