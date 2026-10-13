module box_m

use iso_c_binding
use globals
public

type, bind(C) :: box_px_t
   integer(c_int64_t) ::  x, y, w, h
end type

type, bind(C) :: box_stats_t
   real(c_double) :: x, y
   real(buf_k) :: av, sd
end type

end module
