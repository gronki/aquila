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
   character(len=16) :: option

   call get_command_argument(1, option)
   if (option /= "run") then
      write (*, "(a)") "This is a benchmark, not run during normal testing"
      stop
   end if

   call time_conv(run_ref, ttotal_ref)
   write (*, fmt) "reference", ttotal_ref, 1.0
   call time_conv(run_pad, ttotal)
   write (*, fmt) "pad", ttotal, ttotal_ref / ttotal
   call time_conv(run_nopad, ttotal)
   write (*, fmt) "nopad", ttotal, ttotal_ref / ttotal
   !call time_conv(run_pad_para, ttotal)
   !write (*, fmt) "pad_para", ttotal, ttotal_ref / ttotal
   !call time_conv(run_nopad_para, ttotal)
   !write (*, fmt) "nopad_para", ttotal, ttotal_ref / ttotal

contains

   subroutine time_conv(conv_runner, ttotal)
      procedure(run_conv) :: conv_runner

      real(real64) :: t0, t1, ttotal
      real(real32), allocatable :: x(:,:), k(:,:), kp(:,:), y(:,:)
      integer :: ix, ik, ikrep
      integer(int64), parameter :: xsizes(*,*) = reshape([ &
      &        888, 768  , &
      &       1224, 963  , &
      &       2424, 1963 , &
      &       3524, 2963 , &
      &       6048, 4096  ], shape=[2, 5])
      integer(int64), parameter :: ksizes(*,*) = reshape([ &
      &       3 ,  3 ,  &
      &       5 ,  5 ,  &
      &       7 ,  7 ,  &
      &       9 ,  9 ,  &
      &       11,  11, &
      &       13,  13  ], shape=[2, 6])
      integer(int64) :: xpixels, kpixels, flo, krep

      ttotal = 0
      do ix = 1, size(xsizes, 2)
         if (allocated(x)) deallocate(x)
         if (allocated(y)) deallocate(y)

         allocate(x(xsizes(1, ix), xsizes(2, ix)))
         allocate(y(xsizes(1, ix), xsizes(2, ix)))
         xpixels = xsizes(1, ix) * xsizes(2, ix)

         call random_number(x)

         do ik = 1, size(ksizes, 2)
            if (allocated(k)) deallocate(k)

            kpixels = ksizes(1, ik) * ksizes(2, ik)
            allocate(k(ksizes(1, ik), ksizes(2, ik)))
            call random_number(k)

            kp = padded_2d_kernel(k, 8_size_k)

            krep = max(1_int64, int(128.0 / kpixels * 3000. * 3000. / xpixels, int64))
            call cpu_time(t0)
            do ikrep = 1, krep
               call conv_runner(x, k, kp, y)
            end do
            call cpu_time(t1)

            flo = krep * ksizes(1, ik) * (xsizes(1, ix) - ksizes(1,ik) + 1) &
               * ksizes(2, ik) * (xsizes(2, ix) - ksizes(2,ik) + 1)

            print '(a, 2i6, a, 2i3, a, i4, a, f5.3, a, f7.2)', &
               "image_shape = ", xsizes(:,ix), &
               " kernel_shape = ", ksizes(:,ik), &
               " rep =", krep, &
               " time =", t1-t0, " GFLOPS=", flo / (t1 - t0) / 1e9_real64
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

   subroutine run_nopad(x, k, kp, y)
      real(real32), contiguous :: x(:,:), k(:,:), kp(:,:), y(:,:)
      call conv2d_nopad(x, k, .true., y)
   end subroutine


   subroutine run_pad_para(x, k, kp, y)
      real(real32), contiguous :: x(:,:), k(:,:), kp(:,:), y(:,:)
      call conv2d_pad(x, kp, size(k, 1, kind=size_k), .true., y, .true.)
   end subroutine

   subroutine run_nopad_para(x, k, kp, y)
      real(real32), contiguous :: x(:,:), k(:,:), kp(:,:), y(:,:)
      call conv2d_nopad(x, k, .true., y, .true.)
   end subroutine


end program
