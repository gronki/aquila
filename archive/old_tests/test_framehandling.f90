program test_framehandling

  use framehandling
  use globals
  implicit none

  integer :: errno

  block
    type(frame_t) :: f
    print *, ' -- test 1'
    call f % read_fits('veil-1.fits')
    print *, 'shape(f % data) = ', shape(f % data)
  end block

  block
    type(frame_t) :: f1
    type(image_frame_t) :: f2
    print *, ' -- test 2'
    call f1 % read_fits('veil-1.fits')
    print *, 'shape(f1 % data) = ', shape(f1 % data)
    call f2 % assign_frame(f1)
    print *, 'shape(f2 % data) = ', shape(f2 % data)
  end block

  block
    type(frame_t) :: f1
    type(image_frame_t) :: f2
    print *, ' -- test 3'
    call f1 % read_fits('veil-1.fits')
    print *, 'shape(f1 % data) = ', shape(f1 % data)
    f2 % data => f1 % data
    print *, 'shape(f2 % data) = ', shape(f2 % data)
  end block

  block
    type(frame_t) :: f1
    type(image_frame_t) :: f2
    print *, ' -- test 4'
    call f1 % read_fits('veil-1.fits')
    print *, 'shape(f1 % data) = ', shape(f1 % data)
    call f2 % assign_data( f1 % data )
    print *, 'shape(f2 % data) = ', shape(f2 % data)
  end block


  block
    use fitsheader_m
    type(fhdict) :: d
    character(len = 80) :: a
    print *, ' -- test 6'
    call d % add('key', 'val')
    call d % add('a', 'b')
    call d % add('EXPTIME', 60.0)
    call d % add('negexp', -7e-11)
    call d % add('opt', -33)
    print *, d
    a = d % get_str('key')
    print *, 'key is ', a
    print *, 'EXPTIME = ', d % get_real('EXPTIME')
    print *, 'opt = ', d % get_int('opt')
    ! print *, 'negexp = ', d % get_int('negexp', errno)
    ! print *, 'errno = ', errno, errno /= 0
    ! print *, 'negexp = ', d % get_real('negexp', errno)
    ! print *, 'errno = ', errno, errno == 0
    print *, 'negexp .in. d', 'negexp' .in. d
    print *, 'd % has_key(negexp)', d % has_key('negexp')
    print *, 'niema .in. d', 'niema' .in. d
  end block

  block
    type(image_frame_t) :: f1
    print *, ' -- test 5'
    call f1 % read_fits('veil-1.fits')
    print *, 'shape(f1 % data) = ', shape(f1 % data)
    print *, f1 % hdr
    print *, 'EXPTIME = ', f1 % hdr % get_real('EXPTIME')
  end block



  print *, ' -- end'
end program
