program test_convolutions

   use fastconv
   use convolutions

   call test_fastconv
   call test_fastconv_case2
   call test_padding
   call test_padding_case2

contains

   subroutine test_fastconv

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

      real(real_k) :: out1(3,3), out2(3,3), out3(3,3), out1f(5,5), out2f(5,5), out3f(5,5)

      call conv2d_ref(inp, k, .false., out1)
      call conv2d_pad(inp, padded_2d_kernel(k, 8_size_k), size(k, 1, size_k), .false., out2)
      call conv2d_nopad(inp, k, .false., out3)

      call printarr(out1)
      print *
      call printarr(out2)
      print *
      call printarr(out3)

      if (any(abs(out1 - out2) > 1e-3)) error stop
      if (any(abs(out1 - out3) > 1e-3)) error stop

      out1f(:,:) = 0
      out2f(:,:) = 0
      out3f(:,:) = 0

      call conv2d_ref(inp, k, .true., out1f)
      call conv2d_pad(inp, padded_2d_kernel(k, 8_size_k), size(k, 1, size_k), .true., out2f)
      call conv2d_nopad(inp, k, .true., out3f)

      print *
      print *
      call printarr(out1f)
      print *
      call printarr(out2f)
      print *
      call printarr(out3f)

      if (any(abs(out1f - out2f) > 1e-3)) error stop
      if (any(abs(out1f - out3f) > 1e-3)) error stop
   end subroutine

   subroutine test_fastconv_case2

      real(real_k) :: inp(5,5) = reshape([ &
         0., 0., 0., 0., 0., &
         0., 1., 0., 0., 0., &
         0., 0., 0., 2., 0., &
         0., 0., 1., 0., 0., &
         0., 0., 0., 0., 0.  &
         ], [5,5])
      real(real_k) :: k(5,5) = reshape([ &
         0., 0., 0., 0., 0., &
         0., 1., 0., 0., 0., &
         0., 0., 0., 2., 0., &
         0., 0., 1., 0., 0., &
         0., 0., 0., 0., 0.  &
         ], [5,5])

      real(real_k) :: out1(1,1), out2(1,1), out3(1,1), out1f(5,5), out2f(5,5), out3f(5,5)

      call conv2d_ref(inp, k, .false., out1)
      call conv2d_pad(inp, padded_2d_kernel(k, 8_size_k), size(k, 1, size_k), .false., out2)
      call conv2d_nopad(inp, k, .false., out3)

      call printarr(out1)
      print *
      call printarr(out2)
      print *
      call printarr(out3)

      if (any(abs(out1 - out2) > 1e-3)) error stop
      if (any(abs(out1 - out3) > 1e-3)) error stop

      out1f(:,:) = 0
      out2f(:,:) = 0
      out3f(:,:) = 0

      call conv2d_ref(inp, k, .true., out1f)
      call conv2d_pad(inp, padded_2d_kernel(k, 8_size_k), size(k, 1, size_k), .true., out2f)
      call conv2d_nopad(inp, k, .true., out3f)

      print *
      print *
      call printarr(out1f)
      print *
      call printarr(out2f)
      print *
      call printarr(out3f)

      if (any(abs(out1f - out2f) > 1e-3)) error stop
      if (any(abs(out1f - out3f) > 1e-3)) error stop
   end subroutine

   subroutine test_padding
      real(real_k), parameter :: inp(7,7) = reshape([ &
         3., 0., 0., 1., 1., 1., 0., &
         0., 0., 0., 0., 0., 0., 0., &
         0., 0., 0., 0., 0., 0., 0., &
         0., 0., 0., 0., 0., 0., 0., &
         1., 0., 0., 2., 2., 2., 0., &
         0., 1., 0., 0., 0., 0., 1., &
         0., 0., 2., 0., 0., 0., 0.  &
         ], [7, 7])
      real(real_k), parameter :: krn(5,5) = reshape([ &
         1., 0., 0., 1., 0., &
         0., 2., 3., 2., 0., &
         1., 3., 4., 3., 0., &
         1., 2., 2., 0., 1., &
         0., 1., 1., 1., 0.  &
         ], [5, 5])

      real(real_k) :: outp_ref(7,7), inp_ext(11, 11), outp_new(7, 7)
      character(len=12), parameter :: methods(*) = [character(len=12) :: &
         "r", "e", "0", "-1"]
      integer :: imethod

      outp_ref(:,:) = 0
      call conv2d_ref(inp, krn, .true., outp_ref)
      print *, "no padding (ref):"
      call printarr(outp_ref)
      outp_ref(:,:) = 0
      call conv2d_nopad(inp, krn, .true., outp_ref)
      print *, "no padding (nopad):"
      call printarr(outp_ref)


      do imethod = 1, size(methods)

         print *
         print *, " ** TESTING METHOD: ", trim(methods(imethod))

         outp_ref(:,:) = 0
         outp_new(:,:) = 0

         call expand_image(inp, shape(krn, kind=size_k), methods(imethod), inp_ext)
         call conv2d_ref(inp_ext, krn, .false., outp_ref)
         call conv2d_fix(inp, krn, methods(imethod), outp_new)

         print *, 'expanded'
         call printarr(inp_ext)
         print *, 'convolution -- from expanded'
         call printarr(outp_ref)
         print *, 'convolution -- patched'
         call printarr(outp_new)


         if (any(abs(outp_ref - outp_new) > 1e-3)) error stop

      end do
   end subroutine

   subroutine test_padding_case2
      real(real_k), parameter :: inp(5, 5) = reshape([ &
         3., 0., 0., 1., 0., &
         0., 0., 0., 0., 0., &
         0., 0., 0., 0., 0., &
         0., 1., 0., 0., 1., &
         0., 0., 2., 0., 0.  &
         ], [5, 5])
      real(real_k), parameter :: krn(5,5) = reshape([ &
         1., 0., 0., 1., 0., &
         0., 2., 3., 2., 0., &
         1., 3., 4., 3., 0., &
         1., 2., 2., 0., 1., &
         0., 1., 1., 1., 0.  &
         ], [5, 5])

      real(real_k) :: outp_ref(5, 5), inp_ext(9, 9), outp_new(5, 5)
      character(len=12), parameter :: methods(*) = [character(len=12) :: &
         "r", "e", "0", "-1"]
      integer :: imethod

      outp_ref(:,:) = 0
      call conv2d_ref(inp, krn, .true., outp_ref)
      print *, "no padding (ref):"
      call printarr(outp_ref)
      outp_ref(:,:) = 0
      call conv2d_nopad(inp, krn, .true., outp_ref)
      print *, "no padding (nopad):"
      call printarr(outp_ref)


      do imethod = 1, size(methods)

         print *
         print *, " ** TESTING METHOD: ", trim(methods(imethod))

         outp_ref(:,:) = 0
         outp_new(:,:) = 0

         call expand_image(inp, shape(krn, kind=size_k), methods(imethod), inp_ext)
         call conv2d_ref(inp_ext, krn, .false., outp_ref)
         call conv2d_fix(inp, krn, methods(imethod), outp_new)

         print *, 'expanded'
         call printarr(inp_ext)
         print *, 'convolution -- from expanded'
         call printarr(outp_ref)
         print *, 'convolution -- patched'
         call printarr(outp_new)


         if (any(abs(outp_ref - outp_new) > 1e-3)) error stop

      end do
   end subroutine

   subroutine printarr(arr)
      real(real_k) :: arr(:,:)
      integer :: i
      do i = 1, size(arr, 1)
         print '(*(f4.0))', arr(i,:)
      end do
   end subroutine

end program
