module globals

use iso_c_binding
use iso_fortran_env, stderr => error_unit, stdout => output_unit

implicit none

integer, parameter :: r32_k = c_float
integer, parameter :: r64_k = c_double
integer, parameter :: i64_k = c_int64_t
integer, parameter :: i32_k = c_int
integer, parameter :: sz_k = c_size_t
integer, parameter :: buf_k = r32_k

character(len = *), parameter :: hlp_fmt = '(a25, 2x, a)', hlp_fmtc = '(27x, a)'
character(len = *), parameter :: fmthlp = '(a25, 2x, a, :/, *(27x, a, :/))'

character(len = *), parameter :: version = "260207"

character(len = *), parameter :: perf_fmt = '("PERF", a10, 1x, "=", f7.3)'

logical :: cfg_verbose = .false.

contains

subroutine greeting(progname)
   character(len = *), intent(in) :: progname
   print '(*(a))', trim(progname), ' v.', version, ' (https://github.com/gronki/aquila)'
end subroutine

pure function cf(s, f)
   character(len = *), intent(in) :: s
   character(len = *), intent(in), optional :: f
   character(len = :), allocatable :: cf
#   ifdef _WIN32
#   define _NOCOLOR
#   endif
#   ifdef _NOCOLOR
   cf = trim(s)
#   else
   if ( present(f) ) then
      cf = achar(27) // '[' // trim(f) // 'm' // trim(s) // achar(27) // '[0m'
   else
      cf = achar(27) // '[' // '1'     // 'm' // trim(s) // achar(27) // '[0m'
   end if
#   endif
end function

pure function failed(errno)
   integer(c_int), intent(in), optional :: errno
   logical :: failed

   failed = .false.
   if (.not. present(errno)) return
   failed = errno /= 0
end function

pure subroutine pure_require(check, errno, fail_code, fail_msg)
   logical, intent(in) :: check
   integer(c_int), intent(inout), optional :: errno
   integer(c_int), intent(in) :: fail_code
   character(len=*), intent(in), optional :: fail_msg

   if (present(errno)) errno = 0
   if (check) return

   if (present(errno)) then
      errno = fail_code
      return
   end if

   if (present(fail_msg)) error stop fail_msg
   error stop

end subroutine

pure subroutine pure_forbid(check, errno, fail_code, fail_msg)
   logical, intent(in) :: check
   integer(c_int), intent(inout), optional :: errno
   integer(c_int), intent(in) :: fail_code
   character(len=*), intent(in), optional :: fail_msg

   call pure_require(.not. check, errno, fail_code, fail_msg)
end subroutine


function requirement(check, errno, fail_code, fail_msg) result(should_quit)
   logical, intent(in) :: check
   integer(c_int), intent(inout), optional :: errno
   integer(c_int), intent(in) :: fail_code
   character(len=*), intent(in), optional :: fail_msg
   logical :: should_quit

   should_quit = .not. check
   if (present(errno)) errno = 0
   if (check) return

   if (present(fail_msg)) &
      write (stderr, '(a, a)') 'error: ', fail_msg

   if (present(errno)) then
      errno = fail_code
      return
   end if

   if (present(fail_msg)) error stop fail_msg
   error stop
end function

function forbidden(check, errno, fail_code, fail_msg) result(should_quit)
  logical, intent(in) :: check
  integer(c_int), intent(inout), optional :: errno
  integer(c_int), intent(in) :: fail_code
  character(len=*), intent(in), optional :: fail_msg
  logical :: should_quit

  should_quit = requirement(.not. check, errno, fail_code, fail_msg)
end function

end module
