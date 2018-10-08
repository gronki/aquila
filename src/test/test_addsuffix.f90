program test_addsuffix

  use framehandling, only: add_suffix
  implicit none

  character(len = 256) :: fnin, fnout

  !----------------------------------------------------------------------------!

  fnin = "a.fits"
  call add_suffix(fnin, "-r", fnout)
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = "korsarz-b.fits"
  call add_suffix(fnin, "-r", fnout)
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = "a+r.fits"
  call add_suffix(fnin, "-r", fnout)
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = "abc.def.fits"
  call add_suffix(fnin, "-r", fnout)
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = "alfa"
  call add_suffix(fnin, "-r", fnout)
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = ".fits"
  call add_suffix(fnin, "-r", fnout)
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

end program test_addsuffix
