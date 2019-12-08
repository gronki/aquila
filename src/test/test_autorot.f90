program test_autorot

  use new_align
  use globals
  use findstar, only: source

  implicit none

  integer, parameter :: num_stars = 250, n = 5
  real(fp), parameter :: d = 1000, pi = 4 * atan(1.0_fp), reliability = 0.95, &
  &     max_shift = 0.05 * d
  logical :: flip = .false.
  integer :: it, nfail = 0, npass = 0

  type(source), allocatable :: stars0(:), stars1(:)

  call random_seed()

  repeat_experiment: do it = 1, 1000
    block
      real :: a
      call random_number(a)
      flip = a < 0.5
    end block

    print *, 'flip =', flip

    generate_stars: block
      real(fp), dimension(num_stars) :: a
      real(fp) :: dx, dy, dr

      allocate(stars0(size(a)), stars1(size(a)))

      call random_number(a)
      stars0 % x = d / 2 * (2 * a - 1)
      call random_number(a)
      stars0 % y = d / 2 * (2 * a - 1)
      call random_number(a)
      stars0 % flux = 10**(3.0 * a - 1.5)

      call random_number(dx); dx = max_shift * (2 * dx - 1)
      call random_number(dy); dy = max_shift * (2 * dy - 1)
      call random_number(dr); dr = 0.5 * max_shift / (d / 2) * (2 * dr - 1) + merge(pi, 0.0_fp, flip)

      print '(a12,2f9.3,es12.3)', 'ASSUMED =', dx, dy, dr

      call random_number(a)
      stars1 % flux = stars0 % flux * (1 + 0.01 * (2 * a - 1))
      call random_number(a)
      stars1 % x = dx + stars0 % x * cos(dr) - stars0 % y * sin(dr) + 0.25 * (2 * a - 1)
      call random_number(a)
      stars1 % y = dy + stars0 % x * sin(dr) + stars0 % y * cos(dr) + 0.25 * (2 * a - 1)

      call random_number(a)
      stars0 = pack(stars0, a < reliability)
      call random_number(a)
      stars1 = pack(stars1, a < reliability)

    end block generate_stars

    ! write_stars: block
    !   integer :: i
    !   do i = 1, size(stars0)
    !     write (11, '(2f10.2,es12.2)') stars0(i) % x, stars0(i) % y, stars0(i) % flux
    !   end do
    !   do i = 1, size(stars1)
    !     write (12, '(2f10.2,es12.2)') stars1(i) % x, stars1(i) % y, stars1(i) % flux
    !   end do
    ! end block write_stars

    block
      complex, dimension(n,n) :: ft0, ft1, ft01, ft10, fti
      integer :: i
      logical :: pass

      call stars_ft(stars0, real(d), real(d), ft0)
      call stars_ft(stars1, real(d), real(d), ft1)
      ft01(:,:) = ft0 * ft1
      ft10(:,:) = conjg(ft0) * ft1

      ! do i = 1, n
      !   print '(*("(",2f6.2," )",:,2x))', ft0(i,:)
      ! end do
      ! print *, '--------'
      ! do i = 1, n
      !   print '(*("(",2f6.2," )",:,2x))', ft1(i,:)
      ! end do
      ! print *, '--------'
      ! do i = 1, n
      !   print '(*("(",2f6.2," )",:,2x))', ft10(i,:)
      !   write (13, '(*(f9.3))') real(ft10(i,:))
      ! end do

      pass = flip .eqv. sum(real(ft10)) < sum(real(ft01))
      print *, 'TEST', sum(real(ft01)), ' > ', sum(real(ft10)), &
        merge('PASS', 'FAIL', pass)
      if (pass) npass = npass + 1
      if (.not. pass) nfail = nfail + 1
    end block

    deallocate(stars0, stars1)
  end do repeat_experiment

  print *, 'for ', num_stars, ' stars and n=', n, 'got pass/fail = ', npass, '/', nfail
contains

  subroutine stars_ft(stars, wx, wy, ft)
    integer :: i, j
    type(source) :: stars(:)
    complex :: ft(:,:)
    real :: wx, wy
    real :: a(size(stars))

    do concurrent (i = 1:size(ft,1), j = 1:size(ft,2))
      a(:) = 2 * pi * ((i - 1) * stars(:) % x / wx + (j - 1) * stars(:) % y / wy)
      ft(i,j) = sum(stars(:) % flux * cmplx(cos(a(:)), sin(a(:)))) / size(stars)
    end do
  end subroutine

end program
