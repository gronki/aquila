
!----------------------------------------------------------------------------!
!----------------------------------------------------------------------------!
!----------------------------------------------------------------------------!
!----------------------------------------------------------------------------!


program test_matchriangles
  use globals
  use findstar
  use polygon_matching
  
  implicit none

  character(len=256) :: fn1, fn2
  type(extended_source), allocatable :: ls1(:), ls2(:)
  type(polygon), allocatable :: t1(:), t2(:)
  integer :: nmax = 32
  
  if (command_argument_count() /= 2) stop 'supply 2 FITS filenames'
  
  call get_command_argument(1, fn1)
  call register_stars_file(fn1, ls1)
  
  call get_command_argument(2, fn2)
  call register_stars_file(fn2, ls2)

  print *, 'registered stars:', size(ls1), size(ls2)
  print *
  print '(10(es11.3))', ls1 % flux
  print *
  print '(10(es11.3))', ls2 % flux

  call find_starriangles(ls1%source, nmax, t1)
  print *
  call find_starriangles(ls2%source, nmax, t2)
  print *

  block
    real(real64) :: cosrotav, sinrotav, angrotav, transxav, transyav
    type(polygon_match) :: matches(12)

    call match_triangles(t1, t2, matches)
    call process_best_matches(matches, transxav, transyav, angrotav)
    print *, transxav, transyav, angrotav
  end block
  
contains

subroutine register_stars_file(fn, lst)
  use framehandling, only: image_frame_t
  use stacking, only: register_stars
  use findstar, only: extended_source

  character(len=*) :: fn
  type(image_frame_t) :: im
  type(extended_source), allocatable, intent(out) :: lst(:)

  call im % read_fits(fn)
  call register_stars(im%data, lst)
  end subroutine

end program test_matchriangles