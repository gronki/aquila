program test_addsuffix

  use framehandling, only: add_suffix
  implicit none

  character(len = 256) :: fnin, fnout

  !----------------------------------------------------------------------------!

  fnin = "a.fits"
  fnout = add_suffix(fnin, "-r")
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = "korsarz-b.fits"
  fnout = add_suffix(fnin, "-r")
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = "a+r.fits"
  fnout = add_suffix(fnin, "-r")
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = "abc.def.fits"
  fnout = add_suffix(fnin, "-r")
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = "alfa"
  fnout = add_suffix(fnin, "-r")
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

  fnin = ".fits"
  fnout = add_suffix(fnin, "-r")
  write (*, '(a, " -> ", a)') trim(fnin), trim(fnout)

  !----------------------------------------------------------------------------!

end program test_addsuffix
