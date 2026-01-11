module to_str_m

use iso_fortran_env

implicit none

private
public :: to_str

interface to_str
module procedure :: int32_to_str, int64_to_str
module procedure :: real32_to_str, real64_to_str
end interface

contains

pure function int32_to_str(n) result(s)
   integer(int32), intent(in) :: n
   character(len=24) :: buf
   character(len=:), allocatable :: s

   write(buf, *) n
   allocate(s, source=trim(buf))
end function

pure function int64_to_str(n) result(s)
   integer(int64), intent(in) :: n
   character(len=24) :: buf
   character(len=:), allocatable :: s

   write(buf, *) n
   allocate(s, source=trim(buf))
end function

pure function real32_to_str(n) result(s)
   real(real32), intent(in) :: n
   character(len=24) :: buf
   character(len=:), allocatable :: s

   write(buf, *) n
   allocate(s, source=trim(buf))
end function

pure function real64_to_str(n) result(s)
   real(real64), intent(in) :: n
   character(len=24) :: buf
   character(len=:), allocatable :: s

   write(buf, *) n
   allocate(s, source=trim(buf))
end function

end module
