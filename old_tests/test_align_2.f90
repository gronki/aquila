program test_align_2

  use new_align
  use globals
  use findstar, only: source_t

  implicit none

  type(source_t), allocatable :: stars0(:), stars1(:)
  real(8) :: t1, t2

  call random_seed()

  generate_stars: block
    real(fp), dimension(256) :: a
    real(fp) :: dx, dy, dr

    allocate(stars0(size(a)), stars1(size(a)))

    call random_number(a)
    stars0 % x = 500.0 * (2 * a - 1)
    call random_number(a)
    stars0 % y = 500.0 * (2 * a - 1)
    call random_number(a)
    stars0 % flux = 10**(2 * a - 1)

    call random_number(dx); dx = 20 * (2 * dx - 1)
    call random_number(dy); dy = 20 * (2 * dy - 1)
    call random_number(dr); dr = 15 / 500.0 * (2 * dr - 1)

    print '(a12,2f9.3,es12.3)', 'ASSUMED =', dx, dy, dr

    call random_number(a)
    stars1 % flux = stars0 % flux * (1 + 0.02 * (2 * a - 1))
    call random_number(a)
    stars1 % x = dx + stars0 % x * cos(dr) - stars0 % y * sin(dr) + 0.1 * (2 * a - 1)
    call random_number(a)
    stars1 % y = dy + stars0 % x * sin(dr) + stars0 % y * cos(dr) + 0.1 * (2 * a - 1)

    call random_number(a)
    stars0 = pack(stars0, a < 0.9)
    call random_number(a)
    stars1 = pack(stars1, a < 0.9)

    ! print *, 'size(stars0), size(stars1)', size(stars0), size(stars1)
  end block generate_stars

  test_legacy: block
    use legacy_align
    real(fp) :: mx(2,3)

    call cpu_time(t1)
    call align_xyr(stars0, stars1, mx)
    call cpu_time(t2)

    print '(a12,2f9.3,es12.3)', 'LEGACY =', mx(1,1), mx(2,1), asin(mx(2,2))
    write (0, '("PERF =", f9.3)') t2 - t1
  end block test_legacy

  test_new: block
    use new_align
    type(transform_xyr_t) :: v

    v % scale = 500
    call cpu_time(t1)
    call align_gravity(stars0, stars1, v, 2.0_fp)
    call cpu_time(t2)

    print '(a12,2f9.3,es12.3)', 'MODERN =', v % vec(1:2), v % vec(3) / v % scale
    write (0, '("PERF =", f9.3)') t2 - t1
  end block test_new


end program test_align_2
