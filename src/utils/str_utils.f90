module str_utils_m

implicit none (type, external)
public

contains

function replace_extn(fn, suff) result(fn_out)
   character(len = *), intent(in) :: fn, suff
   character(len = :), allocatable :: fn_out
   integer :: idot

   idot = index(fn, '.', back = .True.)
   if (idot == 0) then
      fn_out = trim(fn) // suff
   else
      fn_out = fn(1:idot) // suff
   end if
end function

elemental logical function endswith(buf, suff)
   character(len = *), intent(in) :: buf, suff
   integer :: n
   if (len_trim(buf) >= len_trim(suff)) then
      n = len_trim(buf)
      endswith = buf(n - len_trim(suff) + 1 : n) == trim(suff)
   else
      endswith = .false.
   end if
end function


function add_suffix(fn, suff) result(fn_out)
   character(len = *), intent(in) :: fn, suff
   character(len = :), allocatable :: fn_out
   integer :: idot

   idot = index(fn, '.', back = .True.)
   if (idot == 0) then
      fn_out = trim(fn) // suff
   else if (idot == 1) then
      error stop "this filename is incorrect"
   else
      fn_out = fn(1:idot-1) // trim(suff) // fn(idot:)
   end if
end function

end module
