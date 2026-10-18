module stacking

use globals
use aquila_c_binding
use, intrinsic :: iso_c_binding
use, intrinsic :: ieee_arithmetic, only: ieee_is_normal
implicit none (type, external)

contains


 !----------------------------------------------------------------------------!

pure function check_corners(tx, nx, ny) result(margin)
   use new_align, only: transform_t

   class(transform_t), intent(in) :: tx
   integer, intent(in) :: nx, ny
   real(r64_k) :: rx, ry, sx, sy
   integer :: margin

   rx = 0.5_r64_k * (nx - 1)
   ry = 0.5_r64_k * (ny - 1)

   margin = 0
   call tx % apply(-rx, -ry, sx, sy)
   margin = max(margin, ceiling(abs(abs(rx) - abs(sx))), ceiling(abs(abs(ry) - abs(sy))))
   call tx % apply( rx, -ry, sx, sy)
   margin = max(margin, ceiling(abs(abs(rx) - abs(sx))), ceiling(abs(abs(ry) - abs(sy))))
   call tx % apply( rx,  ry, sx, sy)
   margin = max(margin, ceiling(abs(abs(rx) - abs(sx))), ceiling(abs(abs(ry) - abs(sy))))
   call tx % apply(-rx,  ry, sx, sy)
   margin = max(margin, ceiling(abs(abs(rx) - abs(sx))), ceiling(abs(abs(ry) - abs(sy))))
end function

 !----------------------------------------------------------------------------!

subroutine mask_margins(mask, margin)
   logical, intent(inout) :: mask(:,:)
   integer :: margin, i, j, ni, nj
   if (margin == 0) return
   if (margin < 0) error stop "margin < 0"
   ni = size(mask, 1)
   nj = size(mask, 2)
   mask(:min(margin, ni), :) = .false.
   mask(max(ni - margin + 1, 1):, :) = .false.
   mask(:, :min(margin, nj)) = .false.
   mask(:, max(nj - margin + 1, 1):) = .false.
end subroutine

subroutine mask_margins_c(mask, ni, nj, margin) bind(C, name="mask_margins")
   logical(c_bool), intent(inout) :: mask(:,:)
   integer(c_int64_t) :: ni, nj, margin, i, j
   if (margin == 0) return
   if (margin < 0) error stop "margin < 0"
   mask(:min(margin, ni), :) = .false.
   mask(max(ni - margin + 1, 1):, :) = .false.
   mask(:, :min(margin, nj)) = .false.
   mask(:, max(nj - margin + 1, 1):) = .false.
end subroutine


 !----------------------------------------------------------------------------!

subroutine normalize_offset_gain_c(buffers, buffers_out, n_buf, margin, err) &
   bind(C, name="normalize_offset_gain")
   use statistics, only: linfit, outliers_2d_mask
   use aquila_c_binding

   integer(c_int), value :: n_buf, margin
   type(buffer_descriptor_r32_t) :: buffers(n_buf), buffers_out(n_buf)
   type(error_status_t) :: err

   real(buf_k) :: a, b, av, sd
   real(buf_k), allocatable :: imref(:,:), xx(:), yy(:)
   real(buf_k), pointer, CONTIGUOUS :: ptr(:,:), ptr_out(:,:)
   logical, allocatable :: mask(:,:)
   integer :: i, j, sz(2), nstack
   integer(c_double) :: np

   call reset_err(err)

   do i = 1, n_buf
      ptr => from_descriptor(buffers(i))
      if (i == 1) then
         allocate(imref, source=ptr)
      else
         if (any(shape(imref) /= shape(ptr))) then
            call set_err(err, msg="buffer shape mismatch")
            return
         end if
         imref(:,:) = imref + ptr
      end if
   end do
   imref(:,:) = imref / n_buf
   sz = shape(imref)

   ! create mask which excludes edges and the brigtenst pixels
   allocate(mask(sz(1), sz(2)), source=.true.)
   call mask_margins(mask, margin)
   ! call outliers_2d_mask(imref, mask, 3.0_buf_k, 10, av, sd)
   mask(:,:) = mask .and. imref < (minval(imref, mask=mask) + maxval(imref, mask=mask)) / 2
   mask(:,:) = mask .and. ieee_is_normal(imref)

   ! pack it into 1-d array
   np = count(mask, kind=c_double)
   if (np == 0) then
      call set_err(err, msg="no points eligible for normalization")
      return
   end if
   allocate(xx(np), yy(np))
   xx(:) = pack(imref, mask)
   deallocate(imref)

   do i = 1, n_buf
      ptr => from_descriptor(buffers(i))
      ptr_out => from_descriptor(buffers_out(i))
      yy(:) = pack(ptr(:,:), mask)
      call linfit(xx, yy, a, b)
      write (stderr, '("NORM frame(",i2,") y = ",f5.3,"x + ",f7.1)') i, a, b
      ptr_out(:,:) = (ptr(:,:) - b) / a
   end do
end subroutine

 !----------------------------------------------------------------------------!

subroutine stack_frames(strategy, method, frames, bufs, frame_out)
   use framehandling, only: image_frame_t

   character(len = *), intent(in) :: strategy, method
   type(buffer_descriptor_r32_t), intent(in) :: bufs(:)
   type(image_frame_t), intent(in) :: frames(:)
   type(image_frame_t), intent(out) :: frame_out
   real(real64) :: t1, t2
   integer :: nstack

   nstack = size(bufs)

   call frame_out % check_shape(int(bufs(1) % rows), int(bufs(1) % cols))

   call cpu_time(t1)
   call stack_buffer(method, bufs, frame_out % data)
   call cpu_time(t2)
   print perf_fmt, 'stack', t2 - t1

   write_extra_info_hdr: block

      call frame_out % hdr % add_int('NSTACK', nstack)
      call frame_out % hdr % add_str('STCKMTD', method)
      if (strategy /= '') call frame_out % hdr % add_str('FRAMETYP', strategy)

      call propagate_average_value_real(frames(1:nstack), 'EXPTIME', frame_out, .false.)
      call propagate_average_value_real(frames(1:nstack), 'CCD-TEMP', frame_out, .true.)

      frame_out%exptime = sum(frames(1:nstack)%exptime)

   end block write_extra_info_hdr

   if ((strategy == 'bias' .or. strategy == 'dark') .and. nstack > 1) then
      estimate_noise: block
         real(buf_k) :: rms

         rms = estimate_differential_noise(bufs)

         write (*, '("RMS = ", f10.2)') rms
         call frame_out % hdr % add_real('RMS', real(rms))
         call frame_out % hdr % add_real('STACKRMS', real(rms / sqrt(1.0_buf_k * nstack)))
      end block estimate_noise
   end if

   call frame_out % hdr % add('AQLVER', version)

end subroutine stack_frames

 !----------------------------------------------------------------------------!

subroutine stack_frames_c(frames, n_frames, method_, frame_out, err) &
   bind(C, name="stack_frames")
   use framehandling, only: image_frame_t

   character(kind=c_char, len = 1), intent(in) :: method_(*)
   character(len=32) :: method
   integer(c_int), value :: n_frames
   type(buffer_descriptor_r32_t), intent(in) :: frames(n_frames)
   type(buffer_descriptor_r32_t), value :: frame_out
   type(error_status_t) :: err
   real(buf_k), pointer, contiguous :: buf_out(:,:)
   real(real64) :: t1, t2
   integer :: i

   call reset_err(err)

   call c_f_string(method_, method)

   do i = 1, n_frames
      if (.not. c_associated(frames(i) % data)) then
         call set_err(err, code=status_err_empty_buffer, &
            msg="Empty buffer encountered during stacking")
         return
      end if
      if (frames(i)%rows /= frame_out%rows .or. frames(i)%cols /= frame_out%cols) then
         call set_err(err, code = status_err_shape_mismatch, msg = "mismatch of frame dimensions")
         return
      end if
   end do

   buf_out => from_descriptor(frame_out)

   call cpu_time(t1)
   call stack_buffer(method, frames, buf_out)
   call cpu_time(t2)

   print perf_fmt, 'stack', t2 - t1

end subroutine stack_frames_c

 !----------------------------------------------------------------------------!

function select_output_filename(output_fn, strategy) result(output_fn_clean)
   character(len=*), intent(in) :: output_fn, strategy
   character(len=:), allocatable :: output_fn_clean

   if (output_fn == "") then
      if (strategy /= "") then
         output_fn_clean = trim(strategy) // ".fits"
      else
         output_fn_clean = "out.fits"
      end if
   else
      output_fn_clean = output_fn
   end if
end function

 !----------------------------------------------------------------------------!

subroutine stack_buffer(method, bufs, buffer_out)
   use statistics, only: quickselect, sigclip2

   type(buffer_descriptor_r32_t), intent(in) :: bufs(:)
   real(buf_k), intent(out) :: buffer_out(:,:)
   character(len = *), intent(in) :: method
   integer :: i, j, k, ni, nstack
   real(buf_k) :: a(size(bufs))
   ! column j of every frame: col(:, k) = frame k, column j
   real(buf_k), allocatable :: col(:,:)
   real(buf_k), pointer, contiguous :: buf(:,:)

   nstack = size(bufs)
   ni = size(buffer_out, 1)

   !$omp parallel do private(i, j, k, a, col, buf)
   do j = 1, size(buffer_out, 2)
      allocate(col(ni, nstack))
      do k = 1, nstack
         buf => from_descriptor(bufs(k))
         col(:, k) = buf(:, j)
      end do

      select case (method)
      case ('m', 'median')
         do i = 1, ni
            a(:) = col(i, :)
            buffer_out(i, j) = quickselect(a(:), (nstack + 1) / 2)
         end do
      case ('s', 'sigclip')
         do i = 1, ni
            buffer_out(i, j) = sigclip2(col(i, :), 3._buf_k)
         end do
      case default
         do i = 1, ni
            buffer_out(i, j) = sum(col(i, :)) / nstack
         end do
      end select

      deallocate(col)
   end do
   !$omp end parallel do
end subroutine

 !----------------------------------------------------------------------------!

subroutine propagate_average_value_real(frames, kw, frame_out, average)
   use framehandling, only: image_frame_t

   class(image_frame_t), intent(in) :: frames(:)
   class(image_frame_t), intent(inout) :: frame_out
   logical, intent(in) :: average
   character(len = *) :: kw
   logical :: m(size(frames))
   real :: av
   integer :: i

   m(:) = [ (frames(i) % hdr % has_key(kw), i = 1, size(frames)) ]
   if (count(m) > 0) then
      av = sum([ (merge(frames(i) % hdr % get_real(kw, 0.0), 0.0, m(i)), &
      &     i = 1, size(frames)) ]) / merge(count(m), 1, average)
      call frame_out % hdr % add_real(kw, av)
   end if
end subroutine

 !----------------------------------------------------------------------------!

function estimate_differential_noise(bufs) result(rms)
   use iso_fortran_env, only: int64
   type(buffer_descriptor_r32_t), intent(in) :: bufs(:)
   real(buf_k) :: rms, av(size(bufs))
   real(buf_k), pointer, contiguous :: b1(:,:), b2(:,:)
   integer :: i, n
   integer(int64) :: nxny

   nxny = bufs(1) % rows * bufs(1) % cols
   n = size(bufs)

   do i = 1, n
      b1 => from_descriptor(bufs(i))
      av(i) = sum(b1) / nxny
   end do

   rms = 0
   do i = 1, n - 1
      b1 => from_descriptor(bufs(i))
      b2 => from_descriptor(bufs(i + 1))
      rms = rms + sum((b1 - av(i) - b2 + av(i+1))**2) / (2 * nxny)
   end do

   rms = sqrt(rms / (n - 1))
end function

end module stacking
