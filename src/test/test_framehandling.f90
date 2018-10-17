program test_framehandling

  use framehandling
  use globals
  implicit none

  integer :: i

  boo: block
    type(image_frame_t), target :: fr
    class(frame_t), pointer :: bf
    integer :: nx, ny
    character(len = *), parameter :: fn = 'veil-1.fits'
    bf => fr
    call read_fits_naxes(fn, nx, ny)
    print *, associated(bf % data)
    allocate(bf % data(nx, ny))
    print *, associated(bf % data)
    call bf % read_fits(fn)
    print *, fr % ccdtemp
    deallocate(bf % data)
    print *, associated(bf % data)
  end block boo




end program
