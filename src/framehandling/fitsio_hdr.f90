module fitsio_hdr_m

use iso_fortran_env
implicit none (type, external)

interface ftppr
subroutine ftppre(unit,group,fpixel,nelements,array,status)
   import :: real32
   integer, intent(in) :: unit, group, fpixel, nelements
   integer, intent(inout) :: status
   real(real32), intent(in) :: array(*)
end subroutine
subroutine ftpprd(unit,group,fpixel,nelements,array,status)
   import :: real64
   integer, intent(in) :: unit, group, fpixel, nelements
   integer, intent(inout) :: status
   real(real64), intent(in) :: array(*)
end subroutine
subroutine ftppri(unit,group,fpixel,nelements,array,status)
   import :: int16
   integer, intent(in) :: unit, group, fpixel, nelements
   integer, intent(inout) :: status
   integer(int16), intent(in) :: array(*)
end subroutine
subroutine ftpprj(unit,group,fpixel,nelements,array,status)
   import :: int32
   integer, intent(in) :: unit, group, fpixel, nelements
   integer, intent(inout) :: status
   integer(int32), intent(in) :: array(*)
end subroutine
subroutine ftpprk(unit,group,fpixel,nelements,array,status)
   import :: int64
   integer, intent(in) :: unit, group, fpixel, nelements
   integer, intent(inout) :: status
   integer(int64), intent(in) :: array(*)
end subroutine
end interface

interface ftgpv
subroutine ftgpve(unit,group,fpixel,nelements,nullval,values,anyf,status)
   import :: real32
   integer, intent(in) :: unit, group, fpixel, nelements
   real(real32), intent(in) :: nullval
   real(real32), intent(inout) :: values(*)
   logical, intent(inout) :: anyf
   integer, intent(inout) :: status
end subroutine
subroutine ftgpvd(unit,group,fpixel,nelements,nullval,values,anyf,status)
   import :: real64
   integer, intent(in) :: unit, group, fpixel, nelements
   real(real64), intent(in) :: nullval
   real(real64), intent(inout) :: values(*)
   logical, intent(inout) :: anyf
   integer, intent(inout) :: status
end subroutine
subroutine ftgpvi(unit,group,fpixel,nelements,nullval,values,anyf,status)
   import :: int16
   integer, intent(in) :: unit, group, fpixel, nelements
   integer(int16), intent(in) :: nullval
   integer(int16), intent(inout) :: values(*)
   logical, intent(inout) :: anyf
   integer, intent(inout) :: status
end subroutine
subroutine ftgpvj(unit,group,fpixel,nelements,nullval,values,anyf,status)
   import :: int32
   integer, intent(in) :: unit, group, fpixel, nelements
   integer(int32), intent(in) :: nullval
   integer(int32), intent(inout) :: values(*)
   logical, intent(inout) :: anyf
   integer, intent(inout) :: status
end subroutine
subroutine ftgpvk(unit,group,fpixel,nelements,nullval,values,anyf,status)
   import :: int64
   integer, intent(in) :: unit, group, fpixel, nelements
   integer(int64), intent(in) :: nullval
   integer(int64), intent(inout) :: values(*)
   logical, intent(inout) :: anyf
   integer, intent(inout) :: status
end subroutine
end interface

interface bitpix
    module procedure :: bitpix_e
    module procedure :: bitpix_d
    module procedure :: bitpix_i
    module procedure :: bitpix_j
    module procedure :: bitpix_k
end interface

contains

function bitpix_e(buf) result(bp)
    real(real32), intent(in) :: buf(..)
    integer :: bp
    bp = -32
end function

function bitpix_d(buf) result(bp)
    real(real64), intent(in) :: buf(..)
    integer :: bp
    bp = -64
end function

function bitpix_i(buf) result(bp)
    integer(int16), intent(in) :: buf(..)
    integer :: bp
    bp = 16
end function

function bitpix_j(buf) result(bp)
    integer(int32), intent(in) :: buf(..)
    integer :: bp
    bp = 32
end function

function bitpix_k(buf) result(bp)
    integer(int64), intent(in) :: buf(..)
    integer :: bp
    bp = 64
end function

end module
