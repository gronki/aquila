module lrgb_m

use aquila_c_binding
use globals
use statistics

implicit none

type, bind(C) :: bkeq_param_t
   logical(c_bool) :: background = .true., stars = .true.
   real(buf_k) :: apar = 2.0, bpar = 0.5, sigma=3.0, sigma_star = 4.0
   integer(c_int) :: niter = 32, margin = 32
end type

contains

subroutine equalize_background_init(param) bind(C)
   type(bkeq_param_t) :: param

   param = bkeq_param_t()
end subroutine

subroutine equalize_background(buffers, nbuf, param, err) bind(C)
   type(buffer_descriptor_t) :: buffers(nbuf)
   type(bkeq_param_t) :: param
   type(error_status_t) :: err
   integer(c_int), value :: nbuf

   integer :: ibuf, nrows, ncols
   logical, allocatable :: mask_bg(:,:), mask_st(:,:)
   real(buf_k), allocatable :: avgbuf(:,:), wbref(:,:), bufnobk(:,:)
   real(buf_k) :: av, sd, bg(nbuf), av_buf(nbuf), sd_buf(nbuf)
   real(buf_k) :: bg_off, wbrefsq, wbcorr
   real(buf_k), pointer, contiguous :: bufdata(:,:)

   call reset_err(err)
   if (nbuf < 1) then
      call set_err(err, msg="At least one buffer required to perform white balance")
      return
   end if

   nrows = buffers(1) % rows
   ncols = buffers(1) % cols

   do ibuf = 1, nbuf
      if (buffers(ibuf) % rows /= nrows .or. buffers(ibuf) % cols /= ncols) then
         call set_err(err, msg="Incompatible buffer dimensions!")
         return
      end if
   end do

   allocate (mask_bg(nrows, ncols), source=.true.)
   mask_bg(1:param%margin, :) = .false.
   mask_bg(nrows - param%margin + 1:, :) = .false.
   mask_bg(:, 1:param%margin) = .false.
   mask_bg(:, ncols - param%margin + 1:) = .false.
   allocate (mask_st, source=mask_bg)

   allocate(avgbuf(nrows, ncols), source=0._buf_k)

   do ibuf = 1, nbuf
      avgbuf(:,:) = avgbuf(:,:) + from_descriptor(buffers(ibuf))
   end do
   avgbuf(:,:) = avgbuf(:,:) / nbuf

   call outliers(avgbuf, mask_bg, param%sigma, param%niter, av, sd)
   mask_st = mask_st .and. (avgbuf >= av + param%sigma_star * sd)

   do ibuf = 1, nbuf
      call avsd(from_descriptor(buffers(ibuf)), mask_bg, av_buf(ibuf), sd_buf(ibuf))
      bg(ibuf) = av_buf(ibuf) - param%apar * sd_buf(ibuf)
   end do

   bg_off = sum(bg) / nbuf * (1 - param%bpar)
   write(*, '("stars ", f4.1, "% surface, background ", f4.1, "%")') &
   &     100 * real(count(mask_st)) / size(mask_st),     &
   &     100 * real(count(mask_bg)) / size(mask_bg)

   write(*, '(a10, " = ", *(f6.1))') 'avg', av_buf
   write(*, '(a10, " = ", *(f6.1))') 'sigma', sd_buf
   write(*, '(a10, " = ", *(f6.1))') ' -> background', bg

   if (param % background) then
      write (*, *) " EQUALIZING BACKGROUND:"
      do ibuf = 1, nbuf
         bufdata => from_descriptor(buffers(ibuf))
         bufdata(:,:) = bufdata - bg(ibuf) + bg_off
         call avsd(bufdata, mask_bg, av_buf(ibuf), sd_buf(ibuf))
         bg(ibuf) = av_buf(ibuf) - param%apar * sd_buf(ibuf)
      end do

      write(*, '(a10, " = ", *(f6.1))') 'avg', av_buf
      write(*, '(a10, " = ", *(f6.1))') 'sigma', sd_buf
      write(*, '(a10, " = ", *(f6.1))') ' -> background', bg
   end if

   if (param % stars) then
      write (*, *) " EQUALIZING STARS:"
      wbref = from_descriptor(buffers(1)) - bg(1)
      wbrefsq = sum(wbref**2, mask_st)
      allocate(bufnobk(nrows, ncols))

      do ibuf = 2, nbuf
         bufdata => from_descriptor(buffers(ibuf))
         bufnobk(:,:) = bufdata-bg(ibuf)
         wbcorr = sum(wbref * bufnobk, mask_st) / wbrefsq
         write (*,'(i0,a,f8.3)') ibuf, ":1 = ", wbcorr
         bufdata(:,:) = bg(ibuf) + bufnobk / wbcorr
      end do
   end if

end subroutine

end module
