module box_m

use iso_c_binding
public

type, bind(C) :: box_px_t
   integer(c_int64_t) ::  x, y, w, h
end type

end module
