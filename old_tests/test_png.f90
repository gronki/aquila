program test_png
  use globals
  use png
  use iso_c_binding
  implicit none
  real(buf_k) :: im(300, 200, 4)
  integer :: i, j

  do concurrent (i = 1:size(im,1), j = 1:size(im,2))
    associate (x => real(i + 25, buf_k) / 50, y => real(j - 11, buf_k) / 50, &
               z => real(i + j - 80, buf_k) / (50 * sqrt(2.0)))
      im(i, j, 1) = 0.6 + 0.4 * cos(2 * 3.14 * x)
      im(i, j, 2) = 0.5 + 0.5 * cos(2 * 3.14 * z)
      im(i, j, 3) = 0.4 + 0.4 * cos(2 * 3.14 * y)
      associate (r => sqrt((x - 4)**2 + (z - 3)**2))
        im(i, j, 4) = 0.5 - 0.5 * cos(2 * 3.14 * r)
      end associate
    end associate
  end do

  call write_png('testpng-1.png', im(:,:,1:1))
  call write_png('testpng-2.png', im(:,:,2:2))
  call write_png('testpng-3.png', im(:,:,3:3))
  call write_png('testpng-4.png', im(:,:,4:4))
  call write_png('testpng-5.png', im(:,:,1:3))
  call write_png('testpng-6.png', im(:,:,2:4:2))
  call write_png('testpng-7.png', im(:,:,1:4))
end program
