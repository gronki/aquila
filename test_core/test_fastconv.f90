program test_Fastconv

   use fastconv

   real(real_k) :: inp(5,5) = reshape([ &
      0., 0., 0., 0., 0., &
      0., 1., 0., 0., 0., &
      0., 0., 0., 2., 0., &
      0., 0., 1., 0., 0., &
      0., 0., 0., 0., 0.  &
      ], [5,5])
   real(real_k) :: k(3,3) = reshape([ &
      0., 0., 1., &
      1., 1., 0., &
      0., 2., 0.  &
      ], [3,3])

   real(real_k) :: out1(3,3), out2(3,3), out1f(5,5), out2f(5,5)

   call conv2d_ref(inp, k, .false., out1)
   call conv2d_pad(inp, padded_2d_kernel(k, 4_size_k), 3_size_k, .false., out2)

   call printarr(out1)
   print *
   call printarr(out2)

   if (any(abs(out1 - out2) > 1e-3)) error stop

   out1f(:,:) = 0
   out2f(:,:) = 0

   call conv2d_ref(inp, k, .true., out1f)
   call conv2d_pad(inp, padded_2d_kernel(k, 4_size_k), 3_size_k, .true., out2f)

   print *
   print *
   call printarr(out1f)
   print *
   call printarr(out2f)

   if (any(abs(out1f - out2f) > 1e-3)) error stop

contains

subroutine printarr(arr)
    real(real_k) :: arr(:,:)
    integer :: i
    do i = 1, size(arr, 1)
        print '(*(f4.0))', arr(i,:)
    end do
end subroutine

end program
