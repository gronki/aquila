module aquila_c_binding

use iso_c_binding
use globals
implicit none

type, bind(c) :: buffer_descriptor_t
   type(c_ptr) :: data
   integer(i64_k) :: rows, cols
end type

type, bind(c) :: error_status_t
   integer(c_int) :: status
   character(kind=c_char, len=1) :: message(64)
end type

integer(c_int), parameter :: status_ok = 0
integer(c_int), parameter :: status_error = 1
integer(c_int), parameter :: status_err_shape_mismatch = 100
integer(c_int), parameter :: status_err_empty_buffer = 101

contains

function check_err(status)
   type(error_status_t) :: status
   logical :: check_err

   check_err = status%status /= status_ok
end function

subroutine reset_err(status)
   type(error_status_t) :: status

   status % status = status_ok
   status % message(1) = achar(0, kind=c_char)
end subroutine

subroutine set_err(status, code, msg)
   type(error_status_t) :: status
   integer(c_int), optional :: code
   character(len=*), optional :: msg

   if (present(code)) then
      status%status = code
   else
      status%status = status_error
   end if

   if (present(msg)) then
      call f_2c_string(msg, status%message, size(status%message))
   end if
end subroutine

function from_descriptor(descr)
   type(buffer_descriptor_t), intent(in) :: descr
   real(buf_k), pointer, contiguous :: from_descriptor(:,:)
   integer(i64_k) :: arrshape(2)

   if (.not. c_associated(descr%data)) then
      nullify(from_descriptor)
      return
   end if

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

subroutine f_2c_string(f_str, c_str, n)
   character(kind=c_char, len=*), intent(in) :: f_str
   character(kind=c_char, len=1), intent(inout) :: c_str(n)
   integer :: n, i, n_min

   n_min = min(n, len(f_str) + 1)

   do i = 1, n_min - 1
      c_str(i) = f_str(i:i)
   end do

   c_str(n_min) = achar(0, c_char)
end subroutine


end module
