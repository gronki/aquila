module source_m
use iso_c_binding
use globals, only: fp
implicit none
public

type, bind(C) :: source_t
   real(c_double) :: x, y
   real(c_double) :: ix, iy
   real(c_double) :: flux = 0
   real(c_double) :: rms = 0
   real(c_double) :: asymmetry_xy, asymmetry_uv, asymmetry
   real(c_double) :: kurtosis
end type

contains


 !----------------------------------------------------------------------------!

elemental subroutine ij_to_xy(i, j, ni, nj, scale, x, y)
   real(fp), intent(in) :: i, j, scale
   integer, intent(in) :: ni, nj
   real(fp), intent(out) :: x, y

   x =   (j - 0.5_fp * (nj + 1)) / scale
   y = - (i - 0.5_fp * (ni + 1)) / scale

end subroutine

!----------------------------------------------------------------------------!

elemental subroutine xy_to_ij(x, y, ni, nj, scale, i, j)
   real(fp), intent(in) :: x, y, scale
   integer, intent(in) :: ni, nj
   real(fp), intent(out) :: i, j

   j =   x * scale + 0.5_fp * (nj + 1)
   i = - y * scale + 0.5_fp * (ni + 1)

end subroutine

end module
