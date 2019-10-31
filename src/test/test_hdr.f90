program test_hdr
  implicit none
  character(len = 128) :: fn, key, val, comment, buf
  integer :: un, status, bsize
  integer :: nkeys, ikey
  character(len = 12), parameter :: excludes(*) = [character(12) :: 'END', 'COMMENT', &
    'SIMPLE', 'BITPIX', 'NAXIS', 'NAXIS1', 'NAXIS2', 'NAXIS3', 'EXTEND', 'BSCALE', 'BZERO']

  read (*,*) fn

  call ftgiou(un, status)
  call ftdkopn(un, fn, 0, bsize, status)

  !-------------------------------------------------------------------------

  do
    call ftghps(un, nkeys, ikey, status)
    print *, ikey > nkeys
    call ftgkyn(un, ikey, key, buf, comment, status)
    if (status /= 0) exit
    if (any(key == excludes)) cycle
    read (buf, *) val
    if (comment /= '') then
      print *, ikey, trim(key), ' = ', trim(val), ' # ', trim(comment)
    else
      print *, ikey, trim(key), ' = ', trim(val)
    end if
  end do

  !-------------------------------------------------------------------------

  call ftclos(un, status)
  call ftfiou(un, status)
end program
