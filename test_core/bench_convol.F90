program bench_convol

   use convolutions
   use fastconv
   use iso_fortran_env

   implicit none (type, external)

   abstract interface
   subroutine run_conv(x, k, kp, y)
      import :: real32
      real(real32), contiguous :: x(:,:), k(:,:), kp(:,:), y(:,:)
   end subroutine
   end interface

   real(real64) :: ttotal_ref, ttotal
   character(len=*), parameter :: fmt = &
      '(a15, " ::: t =", f7.4, " sec; speedup = ", f5.2, "x")'

   call time_conv(run_ref, ttotal_ref)
   write (*, fmt) "reference", ttotal_ref, 1.0
   call time_conv(run_pad, ttotal)
   write (*, fmt) "pad", ttotal, ttotal_ref / ttotal

contains

   subroutine time_conv(conv_runner, ttotal)
      procedure(run_conv) :: conv_runner

      real(real64) :: t0, t1, ttotal
      real(real32), allocatable :: x(:,:), k(:,:), kp(:,:), y(:,:)
      integer :: ix, ik
      integer, parameter :: xsizes(*,*) = reshape([ &
      &        888, 768  , &
      &       1224, 963  , &
      &       2424, 1963 , &
      &       3524, 2963 , &
      &       6048, 4096  ], shape=[2, 5])
      integer, parameter :: ksizes(*,*) = reshape([ &
      &       3,  3 ,  &
      &       5,  5 ,  &
      &       7,  7 ,  &
      &       9,  9 ,  &
      &       11,  11  ], shape=[2, 5])

      ttotal = 0
      do ix = 1, size(xsizes, 2)
         if (allocated(x)) deallocate(x)
         if (allocated(y)) deallocate(y)
         allocate(x(xsizes(1, ix), xsizes(2, ix)))
         allocate(y(xsizes(1, ix), xsizes(2, ix)))
         do ik = 1, size(ksizes, 2)
            if (allocated(k)) deallocate(k)
            allocate(k(ksizes(1, ik), ksizes(2, ik)))
            kp = padded_2d_kernel(k, 8_size_k)
            !print '(a, 2i6, a, 2i3)', "image shape = ", xsizes(:,ix), &
            !   " kernel shape = ", ksizes(:,ik)
            call cpu_time(t0)
            call conv_runner(x, k, kp, y)
            call cpu_time(t1)
            ttotal = ttotal + (t1 - t0)
         end do
      end do


   end subroutine


   subroutine run_ref(x, k, kp, y)
      real(real32), contiguous :: x(:,:), k(:,:), kp(:,:), y(:,:)
      call conv2d_ref(x, k, .true., y)
   end subroutine

   subroutine run_pad(x, k, kp, y)
      real(real32), contiguous :: x(:,:), k(:,:), kp(:,:), y(:,:)
      call conv2d_pad(x, kp, size(k, 1, kind=size_k), .true., y)
   end subroutine

end program
