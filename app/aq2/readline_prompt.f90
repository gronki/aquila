module readline_prompt_m

use runner_m
use iso_c_binding
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
end interface

contains

subroutine readline_init(prompt)
   class(readline_prompt_t), intent(inout) :: prompt

   call c_readline_prompt_init
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

   call c_f_strpointer(input, f_str, nchar)
   line = f_str
   eof = .false.

end subroutine

end module
