module polygon_matching

  use globals
  use iso_fortran_env, only: real64, int64
  use source_m, only: source_t
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

  type :: match_result_t
    real(real64) :: transx(num_poly_vertices), transy(num_poly_vertices)
    real(real64) :: transxav, transyav
    real(real64) :: cosrot(num_poly_vertices), sinrot(num_poly_vertices)
    real(real64) :: cosrotav, sinrotav
    real(real64) :: angrot, varxy, cosine_score
    logical :: passed
  end type

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

if (cfg_verbose) then
      print *, 'n_corners =', num_poly_vertices
      print *, 'nmax =', nmax
      print *, 'ncomb =', ncomb
end if

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
    type(match_result_t) :: result
    type(polygon_match) :: match
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
          
        end if

      end do
    end do

    if (nmatches_cur < nmatches) error stop "insufficient matches between images"

  end subroutine match_polygons

  !--------------------------------------------------------------------------!

  function cosine_coherence(cosrot, sinrot) result(coh)
    real(r64_k) :: cosrot(:), sinrot(:), coh, mx(size(cosrot), size(cosrot))
    integer :: i, j
    do concurrent (i = 1:size(cosrot), j=1:size(cosrot))
      mx(i,j) = cosrot(i) * cosrot(i) + sinrot(i) * sinrot(j)
    end do
    coh = sum(mx) / size(cosrot)**2
  end function


  !--------------------------------------------------------------------------!

  impure elemental subroutine compute_match_transform(match, match_result)
    type(polygon_match), intent(in) :: match
    type(match_result_t), intent(out) :: match_result

    type(polygon_extra) :: tr1, tr2
    integer :: i, j

    tr1 = polygon_extra(match%t1)
    tr2 = polygon_extra(match%t2)

    do i = 1, num_poly_vertices
      match_result%cosrot(i) = tr1%ux(i) * tr2%ux(i) + tr1%uy(i) * tr2%uy(i)
      match_result%sinrot(i) = tr1%ux(i) * tr2%uy(i) - tr1%uy(i) * tr2%ux(i)
    end do

    match_result%sinrotav = sum(match_result%sinrot(:)) / num_poly_vertices
    match_result%cosrotav = sum(match_result%cosrot(:)) / num_poly_vertices

    match_result%angrot = atan2(match_result%sinrotav, match_result%cosrotav)

    do i = 1, num_poly_vertices
      match_result%transx(i) = match%t2%vertices(i)%x &
      - (cos(match_result%angrot) * match%t1%vertices(i)%x &
      - sin(match_result%angrot) * match%t1%vertices(i)%y)
      match_result%transy(i) = match%t2%vertices(i)%y &
      - (sin(match_result%angrot) * match%t1%vertices(i)%x &
      + cos(match_result%angrot) * match%t1%vertices(i)%y)
    end do

    match_result%transxav = sum(match_result%transx(:)) / num_poly_vertices
    match_result%transyav = sum(match_result%transy(:)) / num_poly_vertices

    match_result%varxy = sqrt(sum((match_result%transx(:) - match_result%transxav)**2) &
      + sum((match_result%transy(:) - match_result%transyav)**2) / (2 * num_poly_vertices))
    match_result%cosine_score = cosine_coherence(match_result%cosrot(:), match_result%sinrot(:))

    match_result%passed = ( match_result%cosine_score > 0.999 ) .and. ( match_result%varxy < 1 )

if (cfg_verbose) then
print *, ' ------ POSSIBLE MATCH ------ '
do j = 1, num_poly_vertices
        print '(2i8,2x,"(",2f10.2,")",3x,"(",2f10.2,")")', i, j, match%t1%vertices(j)%x, match%t1%vertices(j)%y, &
        match%t2%vertices(j)%x, match%t2%vertices(j)%y
      end do
      PRINT '(" ch(A)=/", *(f10.2))', match%t1%characteristic
      PRINT '(" ch(B)=/", *(f10.2))', match%t2%characteristic
      
        ! print '(a, 3f7.2, a)', 'rotation = ', atan2(sinrot(k,:), cosrot(k,:)), ' deg'
      print '(a, 3x, *(f8.2))', 'X=', match_Result%transx(:)
      print '(a, 3x, *(f8.2))', 'Y=', match_Result%transy(:)
      print '(a, 3x, *(f8.2))', 'cos=', Match_Result%cosrot(:)
      print '(a, 3x, *(f8.2))', 'sin=', Match_Result%sinrot(:)
      print '(a, 3x, *(f8.2))', 'a=', match_result%angrot
      print *, 'varxy =', match_result%varxy
      print *, 'coherence =', match_result%cosine_score
    if (match_result%passed) then
      print *, '     MATCH APPROVED '
    end if
end if
  end subroutine

  !--------------------------------------------------------------------------!

  subroutine process_best_matches(matches, results, transxav, transyav, angrotav)
    ! find translation and rotation angle based on the best polygon matches

    use statistics, only: sigclip2

    type(polygon_match), intent(in) :: matches(:) ! matches from match_polygons
    type(match_result_t), intent(in) :: results(:) ! matches from match_polygons
    real(real64) :: transxav, transyav, angrotav  ! translation and rotation estimate

    real(real64) :: cosrotav, sinrotav, angrot
    integer :: i, J, k, nmatches

    nmatches = size(matches)

    cosrotav = sigclip2(results%cosrotav, 3.0_real64)
    sinrotav = sigclip2(results%sinrotav, 3.0_real64)
    angrotav = atan2(sinrotav, cosrotav)

if (cfg_verbose) then
    print *, "Matches = ", nmatches
    print *, "cosrotav =", cosrotav
    print *, "sinrotav =", sinrotav
end if

    transxav = sigclip2(results%transxav, 3.0_real64)
    transyav = sigclip2(results%transyav, 3.0_real64)
    
if (cfg_verbose) then
    print '("AVG", 3x, "X=", f9.1, 3x, "Y=", f9.1, 3x, "a=", f9.1)', transxAv, transyav, angrotav*57.3
end if

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
    type(polygon_match), allocatable :: matches(:)
    type(match_result_t), allocatable :: results(:)
    integer i

    allocate(matches(nmatches), results(nmatches))
    
    call find_star_polygons(ls1, nstars, t1)
    call find_star_polygons(ls2, nstars, t2)
    call match_polygons(t1, t2, matches)
    call compute_match_transform(matches, results)
    call process_best_matches(pack(matches, results%passed), &
      pack(results, results%passed), dx, dy, r)
  end subroutine

  !--------------------------------------------------------------------------!

end module
