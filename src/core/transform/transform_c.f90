module transform_c_binding

use transform_m
use projection_m
use transform_xyr_m
use transform_affine_m
use alignment_m
use iso_c_binding

implicit none

type, bind(C) :: transform_c_t
   character(kind=c_char, len=1) :: type(16)
   real(c_double) :: scale = 1
   integer(c_int) :: npar = 0
   real(c_double) :: vec(TRANSFORM_MAX_PAR) = 0
end type

contains

subroutine alloc_transform(typ_str, scale, f_tran)
   character(kind=c_char, len=*), intent(in) :: typ_str
   real(c_double), intent(in) :: scale
   class(transform_t), intent(out), allocatable :: f_tran

   select case (typ_str)
   case ("xyr")
      f_tran = transform_xyr_t(scale)
   case ("affine")
      f_tran = transform_affine_t(scale)
   case default
      error stop "unknown transform"
   end select
end subroutine

subroutine c_to_f_transform(c_tran, f_tran)
   type(transform_c_t), intent(in) :: c_tran
   class(transform_t), intent(out), allocatable :: f_tran

   character(kind=c_char, len=15) :: typ_str
   integer :: npar

   call c_f_string(c_tran%type, typ_str)
   call alloc_transform(typ_str, c_tran%scale, f_tran)

   npar = f_tran%npar()
   if (npar /= c_tran % npar) &
      error stop "wrong number of transform parameters"
   f_tran%vec = c_tran%vec

end subroutine

subroutine f_to_c_transform(f_tran, c_tran)
   class(transform_t), intent(in) :: f_tran
   type(transform_c_t), intent(out) :: c_tran

   character(kind=c_char, len=15) :: typ_str

   select type (f_tran)
   type is (transform_xyr_t)
      typ_str = "xyr"
   type is (transform_affine_t)
      typ_str = "affine"
   class default
      error stop "unsupported type"
   end select

   c_tran%type = transfer(trim(typ_str) // achar(0, kind=c_char), c_tran%type)
   c_tran%scale = f_tran%scale
   c_tran%npar = f_tran%npar()
   c_tran%vec = f_tran%vec

end subroutine

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


subroutine classic_align_c(lst0, n0, lst, n, align_method, params, txc, errno) &
   bind(C, name="classic_align")

   integer(c_size_t), value :: n0, n
   type(source_t), intent(in) :: lst0(n0), lst(n)
   character(kind=c_char, len=1), intent(in) :: align_method(*)
   type(align_params_t), intent(in) :: params
   character(kind=c_char, len=16) :: align_method_f
   type(transform_c_t), intent(out) :: txc
   integer(c_int), intent(out) :: errno

   real(c_double) :: scale
   class(transform_t), allocatable :: tx

   errno = 0

   scale = sum((lst0%x)**2 + (lst0%y)**2)
   scale = sqrt(scale / n0)

   tx = transform_xyr_t(scale)

   call c_f_string(align_method, align_method_f)
   call classic_align(lst0, lst, align_method_f, params, tx, errno)
   call f_to_c_transform(tx, txc)

end subroutine

end module
