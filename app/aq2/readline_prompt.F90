module readline_prompt_m

use runner_m
use iso_c_binding
implicit none(type, external)
private

type, extends(console_prompt_t) :: readline_prompt_t
contains
   procedure :: init => readline_init
   procedure :: input => readline_prompt
end type

public :: readline_prompt_t

interface
subroutine c_readline_prompt_init() bind(C, name="readline_prompt_init")
end subroutine
subroutine c_readline_prompt_read(input, nchar) bind(C, name="readline_prompt_read")
   import :: c_ptr, c_size_t
   type(c_ptr), intent(inout) :: input
   integer(c_size_t), intent(out) :: nchar
end subroutine
subroutine c_readline_prompt_free(input) bind(C, name="readline_prompt_free")
   import :: c_ptr
   type(c_ptr), value :: input
end subroutine
function check_input_is_tty() bind(C, name="check_input_is_tty")
   import :: c_int
   integer(c_int) :: check_input_is_tty
end function
end interface

contains

subroutine readline_init(prompt)
   class(readline_prompt_t), intent(inout) :: prompt

   call c_readline_prompt_init
   prompt % halt_on_error = check_input_is_tty() == 0
end subroutine

subroutine readline_prompt(prompt, line, eof)
   class(readline_prompt_t) :: prompt
   character(len=*), intent(out) :: line
   logical, intent(out) :: eof
   integer(kind=c_size_t) :: nchar

   type(c_ptr) :: input
   character(kind=c_char, len=:), pointer :: f_str

   call c_readline_prompt_read(input, nchar)

   if (.not. c_associated(input)) then
      eof = .true.
      return
   end if

#  ifdef __GFORTRAN__
   block
      character(kind=c_char, len=1), pointer :: f_str_arr(:)
      integer :: i
      call c_f_pointer(input, f_str_arr, [nchar])
      do i = 1, len(line)
         if (i <= nchar) then
            line(i:i) = f_str_arr(i)
         else
            line(i:i) = " "
         end if
      end do
   end block
#  else
   call c_f_strpointer(input, f_str, nchar)
   line = f_str
#  endif
   eof = .false.

end subroutine

end module
