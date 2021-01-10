module polygon_matching

  use globals
  use iso_fortran_env, only: real64, int64
  use findstar, only: source
  implicit none

  integer, parameter :: n_corners = 3
  integer, parameter :: triangle_nv = 1 + 3 * (n_corners - 1)

  type :: triangle
    type(source) :: s(n_corners)
    real(real64) :: v(triangle_nv) = 0
  end type
    
  type, extends(triangle) :: triangle_extra
    real(real64) :: xc, yc
    real(real64), dimension(n_corners) :: l, ux, uy
  end type

  interface triangle_extra
    procedure triangle_extra_ctor
  end interface

  type :: triangle_match
    type(triangle) :: t1, t2
    real(real64) :: dv(triangle_nv), vs
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

  function triangle_extra_ctor(t) result(self)
    type(triangle) :: t
    type(triangle_extra) :: self
    integer :: i, i_next

    self%triangle = t

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

  subroutine find_starriangles(ls, nmax, trngs)

    use iso_fortran_env, only: int64, real64

    integer :: i1, i2, i3, nmax, i, j
    class(source) :: ls(:)
    integer(int64) :: ncomb, n
    type(triangle), allocatable :: trngs(:)
    type(triangle_extra) :: tr

    if (n_corners /= 3) error stop 'find_starriangles is for n_corners=3'

    ncomb = nmax * (nmax - 1) * (nmax - 2) / 6
    allocate(trngs(ncomb))

    n = 0

    do i1 = 1, nmax
      do i2 = 1, nmax
        do i3 = 1, nmax

          if (n >= size(trngs)) exit
          if (i1 == i2 .or. i2 == i3 .or. i1 == i3) cycle

          trngs(n+1) = triangle(s=[ls(i1), ls(i2), ls(i3)])
          tr = triangle_extra(trngs(n+1))

          ! if (.not. all([( tr%l(i) > tr%l(i+1), i = 1, n_corners-1 )])) cycle
          if (.not. (tr%l(1) > tr%l(2) .and. tr%l(2) > tr%l(3))) cycle

          n = n + 1
          trngs(n)%v(:) = [sum(tr%l(:)), &
            (tr%l(i) / tr%l(i+1),                             i = 1, n_corners-1), &
            (tr%s(i)%flux / tr%s(i+1)%flux,                   i = 1, n_corners-1), &
            (tr%ux(i) * tr%ux(i+1) + tr%uy(i) * tr%uy(i+1),   i = 1, n_corners-1)]
          
        end do
      end do
    end do

    if (n < size(trngs)) trngs = trngs(1:n)
  end subroutine find_starriangles

  !--------------------------------------------------------------------------!

  subroutine match_triangles(t1, t2, matches)
    type(triangle), intent(in) :: t1(:), t2(:)
    type(triangle_match), intent(out) :: matches(:)
    integer :: i, j, k, nmatches_cur, i_worst, nmatches
    real(real64) :: dv(triangle_nv), vs, vs_worst

    nmatches = size(matches)
    nmatches_cur = 0
    i_worst = 0

    do i = 1, size(t1)
      do j = 1, size(t2)
        if (j <= i) cycle

        dv(:) = 2 * (t1(i)%v - t2(j)%v) / (abs(t1(i)%v) + abs(t2(j)%v))
        vs =  sqrt(sum(dv**2) / size(dv))

        if ((nmatches_cur < nmatches) .or. (vs < vs_worst)) then
          if (nmatches_cur < nmatches) then
            nmatches_cur = nmatches_cur + 1
            matches(nmatches_cur) = triangle_match(t1(i), t2(j), dv, vs)
          else
            matches(i_worst) = triangle_match(t1(i), t2(j), dv, vs)
          end if
          
          i_worst = maxloc(matches(1:nmatches_cur) % vs, 1)
          vs_worst = matches(i_worst) % vs
          
#         ifdef _DEBUG          
          print '(*(f7.3))', matches % vs
#         endif
        end if

      end do
    end do

    if (nmatches_cur < nmatches) error stop
  
  end subroutine match_triangles

  !--------------------------------------------------------------------------!

  subroutine process_best_matches(matches, transxav, transyav, angrotav)
    use statistics, only: sigclip2

    type(triangle_match), intent(in) :: matches(:)
    type(triangle_extra) :: tr1, tr2
    real(real64), dimension(size(matches), n_corners) :: cosrot, sinrot, transx, transy
    real(real64), dimension(size(matches)) :: angrot
    real(real64) :: cosrotav, sinrotav, angrotav, transxav, transyav
    integer :: i, k, nmatches

    nmatches = size(matches)

    do k = 1, nmatches
      associate (match => matches(k), t1 => matches(k)%t1, t2=>matches(k)%t2)

#       ifdef _DEBUG
        print '(a, *(f8.3))', 'MATCH', match%dv, match%vs
        print '(a, 3("S=",3f9.1,1x), "V=", *(es11.3))', 'T1', t1
        print '(a, 3("S=",3f9.1,1x), "V=", *(es11.3))', 'T2', t2
#       endif

        tr1 = triangle_extra(t1)
        tr2 = triangle_extra(t2)

        do i = 1, n_corners
          cosrot(k,i) = tr1%ux(i) * tr2%ux(i) + tr1%uy(i) * tr2%uy(i)
          sinrot(k,i) = tr1%ux(i) * tr2%uy(i) - tr1%uy(i) * tr2%ux(i)
        end do

#       ifdef _DEBUG
        print '(a, 3f7.2, a)', 'rotation = ', atan2(sinrot(k,:), cosrot(k,:)), ' deg'
#       endif
      end associate
    end do

    cosrotav = sigclip2(sum(cosrot, 2) / n_corners, 3.0_real64)
    sinrotav = sigclip2(sum(sinrot, 2) / n_corners, 3.0_real64)
    angrotav = atan2(sinrotav, cosrotav)

    angrot(:) = atan2(sum(sinrot, 2) / n_corners, sum(cosrot, 2) / n_corners)
    
    do i = 1, n_corners
      transx(:,i) = matches(:)%t2%s(i)%x - (cos(angrot(:)) * matches(:)%t1%s(i)%x - sin(angrot(:)) * matches(:)%t1%s(i)%y)
      transy(:,i) = matches(:)%t2%s(i)%y - (sin(angrot(:)) * matches(:)%t1%s(i)%x + cos(angrot(:)) * matches(:)%t1%s(i)%y)
    end do

    transxav = sigclip2(sum(transx, 2) / n_corners, 3.0_real64)
    transyav = sigclip2(sum(transy, 2) / n_corners, 3.0_real64)
    
#   ifdef _DEBUG
    print '("ang=", f9.1, 3x, "X=", 3f9.1, 3x, "Y=", 3f9.1)', (angrot(i)*57.3, transx(i,:), transy(i,:), i = 1, nmatches)
    print '("AVG ang=", f9.1, 3x, "X=", f9.1, 3x, "Y=", f9.1)', angrotav*57.3, transxAv, transyav
#   endif

  end subroutine process_best_matches

  !--------------------------------------------------------------------------!

  subroutine find_transform_polygons(ls1, ls2, nstars, nmatches, dx, dy, r)
    integer, intent(in) :: nstars, nmatches
    class(source), intent(in) :: ls1(:), ls2(:)
    type(triangle), allocatable :: t1(:), t2(:)
    real(real64) :: dx, dy, r
    type(triangle_match) :: matches(nmatches)
    
    call find_starriangles(ls1, nstars, t1)
    call find_starriangles(ls2, nstars, t2)

    call match_triangles(t1, t2, matches)
    call process_best_matches(matches, dx, dy, r)
  end subroutine

  !--------------------------------------------------------------------------!

end module
