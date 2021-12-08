module polygon_matching

  use globals
  use iso_fortran_env, only: real64, int64
  use findstar, only: source
  implicit none

  integer, parameter :: n_corners = 4
  integer, parameter :: polygon_nv = 1 + 2 * (n_corners - 1)

  type :: polygon
    type(source) :: s(n_corners)
    real(real64) :: v(polygon_nv) = 0
  end type
    
  type, extends(polygon) :: polygon_extra
    real(real64) :: xc, yc
    real(real64), dimension(n_corners) :: l, ux, uy
  end type

  interface polygon_extra
    procedure polygon_extra_ctor
  end interface

  type :: polygon_match
    type(polygon) :: t1, t2
    real(real64) :: dv(polygon_nv), vs
  end type

  private :: cycl

contains

  !--------------------------------------------------------------------------!

  elemental function cycl(i, n)
    integer, intent(in) :: i, n
    integer :: cycl
    cycl = mod(i - 1, n) + 1
  end function

  !--------------------------------------------------------------------------!

  function polygon_extra_ctor(t) result(self)
    type(polygon) :: t
    type(polygon_extra) :: self
    integer :: i, i_next

    self%polygon = t

    self%xc = sum(t%s(:)%x) / n_corners
    self%yc = sum(t%s(:)%y) / n_corners

    do i = 1, n_corners
      i_next = cycl(i + 1, n_corners)
      self%l(i) = hypot(t%s(i_next)%x - t%s(i)%x, t%s(i_next)%y - t%s(i)%y)
      self%ux(i) = (t%s(i_next)%x - t%s(i)%x) / self%l(i)
      self%uy(i) = (t%s(i_next)%y - t%s(i)%y) / self%l(i)
    end do
  end function

  !--------------------------------------------------------------------------!

  pure logical function unique_int(arr) result(is_unique)
    integer, intent(in) :: arr(:)
    integer :: i

    is_unique = .true.
    if (size(arr) < 2) return

    do i = 1, size(arr)-1
      if (any(arr(i+1:) == arr(i))) then
        is_unique = .false.
        return
      end if
    end do
  end function

  !--------------------------------------------------------------------------!

  subroutine find_star_polygons(ls, nmax, polys)

    use iso_fortran_env, only: int64, real64

    integer :: i1, i2, i3, i4, nmax, i, j
    class(source) :: ls(:)
    integer(int64) :: ncomb, n
    type(polygon), allocatable :: polys(:)
    type(polygon_extra) :: tr

    if (n_corners /= 4) error stop 'find_star_polygons is for fixed n_corners'

    ncomb = product([( int(nmax - (i - 1), kind(ncomb)), i = 1, n_corners )]) &
          / product([( i, i = 1, n_corners )])

#   ifdef _DEBUG
      print *, 'n_corners =', n_corners
      print *, 'nmax =', nmax
      print *, 'ncomb =', ncomb
#   endif

    allocate(polys(ncomb))

    n = 0

    do i1 = 1, nmax
    do i2 = 1, nmax
    do i3 = 1, nmax
    do i4 = 1, nmax

      if (n >= size(polys)) exit
      if (.not. unique_int([i1, i2, i3, i4])) cycle

      polys(n+1) = polygon(s=[ls(i1), ls(i2), ls(i3), ls(i4)])
      tr = polygon_extra(polys(n+1))

      if (.not. all([( tr%l(i) > tr%l(i+1), i = 1, n_corners-1 )])) cycle
      ! if (.not. (tr%l(1) > tr%l(2) .and. tr%l(2) > tr%l(3))) cycle

      n = n + 1
      polys(n)%v(:) = [sum(tr%l(:)), &
        (tr%l(i) / tr%l(i+1),                             i = 1, n_corners-1), &
        (tr%ux(i) * tr%ux(i+1) + tr%uy(i) * tr%uy(i+1),   i = 1, n_corners-1)]
          
    end do
    end do
    end do
    end do

    if (n < size(polys)) polys = polys(1:n)
  end subroutine find_star_polygons

  !--------------------------------------------------------------------------!

  subroutine match_polygons(t1, t2, matches)
    type(polygon), intent(in) :: t1(:), t2(:)
    type(polygon_match), intent(out) :: matches(:)
    integer :: i, j, k, nmatches_cur, i_worst, nmatches
    real(real64) :: dv(polygon_nv), vs, vs_worst

    nmatches = size(matches)
    nmatches_cur = 0
    i_worst = 0

    do i = 1, size(t1)
      do j = 1, size(t2)
        if (j <= i) cycle

        dv(:) = 2 * (t1(i)%v - t2(j)%v) / (abs(t1(i)%v) + abs(t2(j)%v))
        vs = norm2(dv)

        if ((nmatches_cur < nmatches) .or. (vs < vs_worst)) then
          if (nmatches_cur < nmatches) then
            nmatches_cur = nmatches_cur + 1
            matches(nmatches_cur) = polygon_match(t1(i), t2(j), dv, vs)
          else
            matches(i_worst) = polygon_match(t1(i), t2(j), dv, vs)
          end if
          
          i_worst = maxloc(matches(1:nmatches_cur) % vs, 1)
          vs_worst = matches(i_worst) % vs
          
#         ifdef _DEBUG          
          print '(*(es9.2))', matches % vs
#         endif
        end if

      end do
    end do

    if (nmatches_cur < nmatches) error stop
  
  end subroutine match_polygons

  !--------------------------------------------------------------------------!

  subroutine process_best_matches(matches, transxav, transyav, angrotav)
    use statistics, only: sigclip2

    type(polygon_match), intent(in) :: matches(:)
    type(polygon_extra) :: tr1, tr2
    real(real64), dimension(size(matches), n_corners) :: cosrot, sinrot, transx, transy
    real(real64) :: cosrotav, sinrotav, angrotav, transxav, transyav, angrot
    integer :: i, k, nmatches

    nmatches = size(matches)

    do k = 1, nmatches
      associate (match => matches(k), t1 => matches(k)%t1, t2=>matches(k)%t2)

#       ifdef _DEBUG
        print '(/, a, es11.3, 8x, *(es11.3))', 'MATCH', match%dv, match%vs
        print '(a, 3x, *("star=",3f9.1,:,2x))', 'T1', t1%s
        print '(a, 3x,   "vect=", *(es11.3))', 'T1', t1%v
        print '(a, 3x, *("star=",3f9.1,:,2x))', 'T2', t2%s
        print '(a, 3x,   "vect=", *(es11.3))', 'T2', t2%v
#       endif

        tr1 = polygon_extra(t1)
        tr2 = polygon_extra(t2)

        do i = 1, n_corners
          cosrot(k,i) = tr1%ux(i) * tr2%ux(i) + tr1%uy(i) * tr2%uy(i)
          sinrot(k,i) = tr1%ux(i) * tr2%uy(i) - tr1%uy(i) * tr2%ux(i)
        end do

        angrot = atan2(sum(sinrot(k,:)) / n_corners, sum(cosrot(k,:)) / n_corners)
    
        do i = 1, n_corners
          transx(k,i) = matches(k)%t2%s(i)%x - (cos(angrot) * matches(k)%t1%s(i)%x - sin(angrot) * matches(k)%t1%s(i)%y)
          transy(k,i) = matches(k)%t2%s(i)%y - (sin(angrot) * matches(k)%t1%s(i)%x + cos(angrot) * matches(k)%t1%s(i)%y)
        end do

#       ifdef _DEBUG
        ! print '(a, 3f7.2, a)', 'rotation = ', atan2(sinrot(k,:), cosrot(k,:)), ' deg'
        print '(a, 3x, *(f8.2))', 'X=', transx(k,:)
        print '(a, 3x, *(f8.2))', 'Y=', transy(k,:)
        print '(a, 3x, *(f8.2))', 'a=', angrot
#       endif
      end associate
    end do

    cosrotav = sigclip2(sum(cosrot, 2) / n_corners, 3.0_real64)
    sinrotav = sigclip2(sum(sinrot, 2) / n_corners, 3.0_real64)
    angrotav = atan2(sinrotav, cosrotav)

    transxav = sigclip2(sum(transx, 2) / n_corners, 3.0_real64)
    transyav = sigclip2(sum(transy, 2) / n_corners, 3.0_real64)
    
#   ifdef _DEBUG
    print '("AVG", 3x, "X=", f9.1, 3x, "Y=", f9.1, 3x, "a=", f9.1)', transxAv, transyav, angrotav*57.3
#   endif

  end subroutine process_best_matches

  !--------------------------------------------------------------------------!

  subroutine find_transform_polygons(ls1, ls2, nstars, nmatches, dx, dy, r)
    integer, intent(in) :: nstars, nmatches
    class(source), intent(in) :: ls1(:), ls2(:)
    type(polygon), allocatable :: t1(:), t2(:)
    real(real64) :: dx, dy, r
    type(polygon_match) :: matches(nmatches)
    
    call find_star_polygons(ls1, nstars, t1)
    call find_star_polygons(ls2, nstars, t2)

    call match_polygons(t1, t2, matches)
    call process_best_matches(matches, dx, dy, r)
  end subroutine

  !--------------------------------------------------------------------------!

end module
