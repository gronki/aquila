module source_m
use iso_c_binding
use globals
implicit none
public

type, bind(C) :: source_t
   real(r64_k) :: x, y
   real(r64_k) :: ix, iy
   real(r64_k) :: flux = 0
   real(r64_k) :: rms = 0
   real(r64_k) :: asymmetry_xy, asymmetry_uv, asymmetry
   real(r64_k) :: kurtosis
end type

contains


 !----------------------------------------------------------------------------!

elemental subroutine ij_to_xy(i, j, ni, nj, scale, x, y)
   real(r64_k), intent(in) :: i, j, scale
   integer(i64_k), intent(in) :: ni, nj
   real(r64_k), intent(out) :: x, y

   x =   (j - 0.5_r64_k * (nj + 1_i64_k)) / scale
   y = - (i - 0.5_r64_k * (ni + 1_i64_k)) / scale

end subroutine

!----------------------------------------------------------------------------!

elemental subroutine xy_to_ij(x, y, ni, nj, scale, i, j)
   real(r64_k), intent(in) :: x, y, scale
   integer(i64_k), intent(in) :: ni, nj
   real(r64_k), intent(out) :: i, j

   j =   x * scale + 0.5_r64_k * (nj + 1_i64_k)
   i = - y * scale + 0.5_r64_k * (ni + 1_i64_k)

end subroutine

end module
