program test_autorot

  use new_align
  use globals
  use source_m, only: source_t

  implicit none

  integer, parameter :: num_stars = 100, n = 4, ntries = 100
  real(fp), parameter :: d = 1000, pi = 4 * atan(1.0_fp), reliability = 0.95, &
  &     max_shift = 0.05 * d
  logical :: flip = .false.
  integer :: it, nfail = 0, npass = 0

  type(source_t), allocatable :: stars0(:), stars1(:)

  call random_seed()

  repeat_experiment: do it = 1, ntries
    block
      real :: a
      call random_number(a)
      flip = a < 0.5
    end block

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

      print '(a12,2f9.3,es12.3)', 'TRANSFORM =', dx, dy, dr

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
      use fftw
      complex(fp), dimension(n,n) :: ft0, ft1, ft01, ft10, ft10a, fti
      integer :: i, j
      logical :: pass
      type(C_PTR) :: plan

      call stars_ft(stars0, real(d), real(d), ft0)
      call stars_ft(stars1, real(d), real(d), ft1)
      ft01(:,:) = ft0 * ft1
      ft10(:,:) = conjg(ft0) * ft1

      ft10a(:,:) = ft10; ft10a(1,1) = 0
      plan = fftw_plan_dft_2d(n, n, ft10a, fti, FFTW_BACKWARD, FFTW_ESTIMATE)
      call fftw_execute_dft(plan, ft10a, fti)
      call fftw_destroy_plan(plan)

      if (ntries == 1) then
        print *, '---- ft0 -----'
        do i = 1, n
          print '(*("(",f6.2,1x,f6.2," )",:,2x))', ft0(i,:)
        end do
        print *, '---- ft1 -----'
        do i = 1, n
          print '(*("(",f6.2,1x,f6.2," )",:,2x))', ft1(i,:)
        end do
        print *, '---- ft01 ----'
        do i = 1, n
          print '(*("(",f6.2,1x,f6.2," )",:,2x))', ft01(i,:)
        end do
        print *, '---- ft10 ----'
        do i = 1, n
          print '(*("(",f6.2,1x,f6.2," )",:,2x))', ft10(i,:)
          print '(*("=",f6.2," ",f6.2,"r ",:,2x))', &
            (abs(ft10(i,j)), atan2(ft10(i,j) % im, ft10(i,j) % re) / pi, j = 1, n)
          write (13, *) atan2(ft10(i,:) % im, ft10(i,:) % re) / pi
        end do
        print *, '---- fti -----'
        do i = 1, n
          print '(*("(",f6.2,1x,f6.2," )",:,2x))', fti(i,:)
          write (14, *) abs(fti(i,:))
        end do
      end if

      pass = flip .eqv. sum(real(ft10)) < sum(real(ft01))
      print '(a, 2x, a, 2x, f8.2, a, f8.2, 5x,a)', 'TEST', merge('FLIP','STR8',flip), &
        sum(real(ft01)) - real(ft10(1,1)), ' > ', sum(real(ft10)) - real(ft10(1,1)),  &
        merge('PASS', 'FAIL', pass)
      if (pass) npass = npass + 1
      if (.not. pass) nfail = nfail + 1
    end block

    deallocate(stars0, stars1)
  end do repeat_experiment

  print '(a, i4, a, i3, a, i4, a, i4)', 'for ', num_stars, ' stars and n=', &
  & n, ' got pass/fail = ', npass, ' /', nfail

contains

  subroutine stars_ft(stars, wx, wy, ft)
    integer :: i, j
    type(source_t) :: stars(:)
    complex(fp) :: ft(:,:)
    real :: wx, wy
    real(fp) :: a(size(stars))

    do concurrent (i = 1:size(ft,1), j = 1:size(ft,2))
      a(:) = 2 * pi * ((i - 1) * stars(:) % x / wx + (j - 1) * stars(:) % y / wy)
      ft(i,j) = sum(stars(:) % flux * cmplx(cos(a(:)), sin(a(:)), fp)) / size(stars)
    end do
  end subroutine

end program
