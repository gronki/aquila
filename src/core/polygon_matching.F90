module polygon_matching

  use globals
  use iso_fortran_env, only: real64, int64
  use findstar, only: source_t
  implicit none

  ! number of polygon vertices
  integer, parameter :: num_poly_vertices = 4
  ! length of the characteristic vector
  integer, parameter :: num_poly_charac_vector = 1 + 2 * (num_poly_vertices - 1)

  type :: polygon
    type(source_t) :: vertices(num_poly_vertices)
    real(real64) :: characteristic(num_poly_charac_vector) = 0
  end type
    
  type, extends(polygon) :: polygon_extra
    real(real64) :: xc, yc
    real(real64), dimension(num_poly_vertices) :: l, ux, uy
  end type

  interface polygon_extra
    procedure polygon_extra_ctor
  end interface

  type :: polygon_match
    type(polygon) :: t1, t2
    real(real64) :: dv(num_poly_charac_vector), vs
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

    self%xc = sum(t%vertices(:)%x) / num_poly_vertices
    self%yc = sum(t%vertices(:)%y) / num_poly_vertices

    do i = 1, num_poly_vertices
      i_next = cycl(i + 1, num_poly_vertices)
      self%l(i) = hypot(t%vertices(i_next)%x - t%vertices(i)%x, t%vertices(i_next)%y - t%vertices(i)%y)
      self%ux(i) = (t%vertices(i_next)%x - t%vertices(i)%x) / self%l(i)
      self%uy(i) = (t%vertices(i_next)%y - t%vertices(i)%y) / self%l(i)
    end do
  end function

  !--------------------------------------------------------------------------!

  pure subroutine next_combin(c, n)
    ! given a vector of integers [1..n, 1..n, ...], generate the next vector
    ! the first call should be applied to c=[1,1,...], and the last resulting
    ! vector will be [n,n,...], after which it will overlow to [1,1,...].

    integer, intent(inout) :: c(:) ! vector of numbers in range 1..n to be bumped
    integer, intent(in) :: n       ! maximum number

    integer :: i

    do i = 1, size(c)
      if (c(i) < n) then
        c(i) = c(i) + 1
        exit
      end if
      c(i) = 1
    end do
  end subroutine

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

  pure logical function is_descending(arr)
    ! check if a sequence is strictly descending

    real(kind=real64), intent(in) :: arr(:) ! sequence to be analyzed
    integer :: i

    is_descending = .true.
    if (size(arr) < 2) return

    do i = 1, size(arr) - 1
      if (arr(i) <= arr(i + 1)) then
        is_descending = .false.
        return
      end if
    end do

  end function

  !--------------------------------------------------------------------------!

  subroutine find_star_polygons(ls, nmax, polys)
    ! finds all possible polygons given the list of stars

    use iso_fortran_env, only: int64, real64

    type(source_t) :: ls(:)                 ! list of sources
    integer :: nmax                        ! how many sources to analyze
    type(polygon), allocatable :: polys(:) ! output list of polygons
    type(source_t) :: tmp_vertices(num_poly_vertices)

    integer :: i
    integer :: indices(num_poly_vertices)
    integer(int64) :: ncomb, n
    type(polygon_extra) :: tr

    ! how many combinations to pick n_corners ordered points?
    ncomb = product([( int(nmax - (i - 1), kind(ncomb)), i = 1, num_poly_vertices )]) &
          / product([( i, i = 1, num_poly_vertices )])

#   ifdef _DEBUG
      print *, 'n_corners =', num_poly_vertices
      print *, 'nmax =', nmax
      print *, 'ncomb =', ncomb
#   endif

    allocate(polys(ncomb))

    n = 1
    indices(:) = 1

    do

      if (n > size(polys)) exit

      ! call the next combination of indices, skip if any two indices are equal
      call next_combin(indices, nmax)
      if (.not. unique_int(indices)) cycle

      ! create a polygon from the stars
      ! loop workaround because of gfortran ICE
      do i = 1, num_poly_vertices
        tmp_vertices(i) = ls(indices(i))
      end do
      polys(n) = polygon(vertices=tmp_vertices)
      tr = polygon_extra(polys(n))

      ! check if sections between points are of increasing length 
      ! to avoid duplicates
      if (.not. is_descending(tr%l)) cycle


      ! construct the characteristic vector for the polygon
      polys(n)%characteristic(:) = [sum(tr%l(:)), &
        (tr%l(i) / tr%l(i+1),                             i = 1, num_poly_vertices-1), &
        (tr%ux(i) * tr%ux(i+1) + tr%uy(i) * tr%uy(i+1),   i = 1, num_poly_vertices-1)]
          
      n = n + 1
    
    end do

    ! n is equal to number of found polygons + 1
    if (n-1 < size(polys)) polys = polys(1:n-1)
  end subroutine find_star_polygons

  !--------------------------------------------------------------------------!

  subroutine match_polygons(t1, t2, matches)
    ! analyze two lists of polygons, and find the best matching ones, based
    ! of a vector of translation, rotation and scale independent characteristics.

    type(polygon), intent(in) :: t1(:), t2(:)       ! two polygon lists
    type(polygon_match), intent(out) :: matches(:)  ! best polygon matches

    integer :: i, j, nmatches_cur, i_worst, nmatches
    real(real64) :: dv(num_poly_charac_vector), vs, vs_worst

    nmatches = size(matches)
    nmatches_cur = 0
    i_worst = 0

    do i = 1, size(t1)
      do j = 1, size(t2)
        if (j <= i) cycle

        ! the difference between the characteristic vectors of two polygons
        dv(:) = 2 * (t1(i)%characteristic - t2(j)%characteristic) &
              / (abs(t1(i)%characteristic) + abs(t2(j)%characteristic))
        ! the measure of a difference
        vs = norm2(dv)

        if ((nmatches_cur < nmatches) .or. (vs < vs_worst)) then
          if (nmatches_cur < nmatches) then
            ! if less than max matches, add regardless of the score
            nmatches_cur = nmatches_cur + 1
            matches(nmatches_cur) = polygon_match(t1(i), t2(j), dv, vs)
          else
            ! if we have max matches, replace the worst match
            matches(i_worst) = polygon_match(t1(i), t2(j), dv, vs)
          end if
          
          ! after updating, find the next worst match
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
    ! find translation and rotation angle based on the best polygon matches

    use statistics, only: sigclip2

    type(polygon_match), intent(in) :: matches(:) ! matches from match_polygons
    real(real64) :: transxav, transyav, angrotav  ! translation and rotation estimate

    type(polygon_extra) :: tr1, tr2
    real(real64), dimension(size(matches), num_poly_vertices) :: cosrot, sinrot, transx, transy
    real(real64) :: cosrotav, sinrotav, angrot
    integer :: i, k, nmatches

    nmatches = size(matches)

    do k = 1, nmatches
      associate (match => matches(k), t1 => matches(k)%t1, t2=>matches(k)%t2)

#       ifdef _DEBUG
        print '(/, a, es11.3, 8x, *(es11.3))', 'MATCH', match%dv, match%vs
        print '(a, 3x, *("star=",3f9.1,:,2x))', 'T1', t1%vertices
        print '(a, 3x,   "vect=", *(es11.3))', 'T1', t1%characteristic
        print '(a, 3x, *("star=",3f9.1,:,2x))', 'T2', t2%vertices
        print '(a, 3x,   "vect=", *(es11.3))', 'T2', t2%characteristic
#       endif

        tr1 = polygon_extra(t1)
        tr2 = polygon_extra(t2)

        do i = 1, num_poly_vertices
          cosrot(k,i) = tr1%ux(i) * tr2%ux(i) + tr1%uy(i) * tr2%uy(i)
          sinrot(k,i) = tr1%ux(i) * tr2%uy(i) - tr1%uy(i) * tr2%ux(i)
        end do

        angrot = atan2(sum(sinrot(k,:)) / num_poly_vertices, sum(cosrot(k,:)) / num_poly_vertices)
    
        do i = 1, num_poly_vertices
          transx(k,i) = matches(k)%t2%vertices(i)%x &
          - (cos(angrot) * matches(k)%t1%vertices(i)%x - sin(angrot) * matches(k)%t1%vertices(i)%y)
          transy(k,i) = matches(k)%t2%vertices(i)%y &
          - (sin(angrot) * matches(k)%t1%vertices(i)%x + cos(angrot) * matches(k)%t1%vertices(i)%y)
        end do

#       ifdef _DEBUG
        ! print '(a, 3f7.2, a)', 'rotation = ', atan2(sinrot(k,:), cosrot(k,:)), ' deg'
        print '(a, 3x, *(f8.2))', 'X=', transx(k,:)
        print '(a, 3x, *(f8.2))', 'Y=', transy(k,:)
        print '(a, 3x, *(f8.2))', 'a=', angrot
#       endif
      end associate
    end do

    cosrotav = sigclip2(sum(cosrot, 2) / num_poly_vertices, 3.0_real64)
    sinrotav = sigclip2(sum(sinrot, 2) / num_poly_vertices, 3.0_real64)
    angrotav = atan2(sinrotav, cosrotav)

    transxav = sigclip2(sum(transx, 2) / num_poly_vertices, 3.0_real64)
    transyav = sigclip2(sum(transy, 2) / num_poly_vertices, 3.0_real64)
    
#   ifdef _DEBUG
    print '("AVG", 3x, "X=", f9.1, 3x, "Y=", f9.1, 3x, "a=", f9.1)', transxAv, transyav, angrotav*57.3
#   endif

  end subroutine process_best_matches

  !--------------------------------------------------------------------------!

  subroutine find_transform_polygons(ls1, ls2, nstars, nmatches, dx, dy, r)
    ! perfrom the polygon-matching estimation of translation and rotation angle
    ! based on a list of detected sources.

    type(source_t), intent(in) :: ls1(:), ls2(:) ! lists of detected sources
    integer, intent(in) :: nstars               ! how many stars to use
    integer, intent(in) :: nmatches             ! how many best matches to analyze
    real(real64), intent(out) :: dx, dy, r      ! output translation and rotation angle

    type(polygon), allocatable :: t1(:), t2(:)
    type(polygon_match) :: matches(nmatches)
    
    call find_star_polygons(ls1, nstars, t1)
    call find_star_polygons(ls2, nstars, t2)

    call match_polygons(t1, t2, matches)
    call process_best_matches(matches, dx, dy, r)
  end subroutine

  !--------------------------------------------------------------------------!

end module
