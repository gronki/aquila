module transform_c_binding

use transform_m
use projection_m
use transform_xyr_m
use alignment_m
use iso_c_binding

implicit none

type, bind(C) :: transform_c_t
   character(kind=c_char, len=1) :: type(8)
   real(c_double) :: scale = 1
   integer(c_int) :: npar = 0
   real(c_double) :: vec(TRANSFORM_MAX_PAR) = 0
end type

contains

subroutine alloc_transform(typ_str, f_tran)
   character(kind=c_char, len=4), intent(in) :: typ_str
   class(transform_t), intent(out), allocatable :: f_tran

   select case (typ_str)
   case ("xyr")
      f_tran = transform_xyr_t()
   case default
      error stop "unknown transform"
   end select
end subroutine

subroutine c_to_f_transform(c_tran, f_tran)
   type(transform_c_t), intent(in) :: c_tran
   class(transform_t), intent(out), allocatable :: f_tran

   character(kind=c_char, len=4) :: typ_str
   integer :: npar

   typ_str = transfer(c_tran%type(:4), typ_str)

   call alloc_transform(typ_str, f_tran)

   f_tran%scale = c_tran%scale
   npar = f_tran%npar()
   if (npar /= c_tran % npar) &
      error stop "wrong number of transform parameters"
   f_tran%vec(:npar) = c_tran%vec(:npar)

end subroutine

subroutine f_to_c_transform(f_tran, c_tran)
   class(transform_t), intent(in) :: f_tran
   type(transform_c_t), intent(out) :: c_tran

   character(kind=c_char, len=4) :: typ_str
   integer :: npar

   select type (f_tran)
   type is (transform_xyr_t)
      typ_str = "xyr"
   class default
      error stop "unsupported type"
   end select

   c_tran%type(:5) = transfer(typ_str // achar(0, kind=c_char), c_tran%type(:5))
   c_tran%scale = f_tran%scale
   npar = f_tran%npar()
   c_tran%npar = npar
   c_tran%vec(:npar) = f_tran%vec(:npar)

end subroutine

subroutine c_f_string(c_str, f_str)
   character(kind=c_char, len=1), intent(in) :: c_str(*)
   character(kind=c_char, len=*), intent(out) :: f_str
   integer :: i

   i = 1
   do while (ichar(c_str(i)) /= 0)
      i = i + 1
   end do

   f_str = transfer(c_str(1:i-1), f_str)
   f_str(i:len(f_str)) = ""

end subroutine


subroutine classic_align_c(lst0, n0, lst, n, align_method, txc, errno) &
   bind(C, name="classic_align")

   integer(c_size_t), value :: n0, n
   type(source_t), intent(in) :: lst0(n0), lst(n)
   character(kind=c_char, len=1), intent(in) :: align_method(*)
   character(kind=c_char, len=16) :: align_method_f
   type(transform_c_t), intent(out) :: txc
   integer(c_int), intent(out) :: errno

   real(c_double) :: scale
   type(transform_xyr_t) :: tx

   errno = 0

   scale = sum((lst0%x)**2 + (lst0%y)**2)
   scale = sqrt(scale / n0)

   tx = transform_xyr_t()
   tx % scale = scale

   call c_f_string(align_method, align_method_f)
   call classic_align(lst0, lst, align_method_f, scale, tx, errno)
   call f_to_c_transform(tx, txc)

end subroutine

end module
