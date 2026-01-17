module starlist_value_m

use findstar
use value_m

private

type, extends(value_t) :: starlist_value_t
   type(source_t), allocatable :: sources(:)
contains
   procedure :: to_str
end type

public :: starlist_value_t

contains

pure function to_str(value) result(str)
   class(starlist_value_t), intent(in) :: value
   character(len=:), allocatable :: str
   character(len=128) :: buf

   if (.not. allocated(value % sources)) then
      str = "(unallocated starlist)"
      return
   end if

   write (buf, '(a, i0, a)') '(', size(value%sources), ' stars)'
   str = trim(buf)

end function

end module
