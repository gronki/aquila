module bkgradient_m

use globals
use iso_c_binding
use aquila_c_binding
use box_m
use statistics
use source_m

implicit none

private

real(c_double), parameter :: golden = (1 + sqrt(5.0_c_double)) / 2

type, bind(C) :: box_sample_param_t
   integer(c_int64_t) :: seed , n_points, window_size, margin
end type

type, bind(C) :: bkgradient_param_t
   real(c_double) :: sd_sigma = 2.0, offset = 0.95, gradient=1.0, sigma_shift = 0
   integer(c_int64_t) :: seed = 1337, n_points = 2048, window_size = 16, margin = 64
   logical(c_bool) :: visualize = .false.
end type

type, bind(C) :: bkgradient_model_t
   real(buf_k) :: a, b, c
end type

public :: bkgradient_param_t, estimate_gradient, default_bkgradient_params

contains

function default_bkgradient_params() bind(C)
   type(bkgradient_param_t) :: default_bkgradient_params
   default_bkgradient_params = bkgradient_param_t()
end function

function next_random(state)
   integer :: state, hi, lo
   real(c_double) :: next_random

   hi = huge(state)
   lo = - huge(state) - 1

   state = 337973 * state + 203767
   next_random = (real(state, c_double) - real(lo, c_double)) &
      / ( real(hi, c_double) - real(lo, c_double))

end function

subroutine sample_point_stats(data_in, param, boxes, box_stats, err_out)

   real(buf_k) :: data_in(:,:)
   type(box_sample_param_t), intent(in) :: param
   type(box_px_t) :: boxes(param%n_points)
   type(error_status_t), intent(inout) :: err_out
   type(box_stats_t) :: box_stats(param%n_points)
   integer :: ibox

   call generate_boxes(shape(data_in, kind=c_int64_t), param, boxes)
   call ij_to_xy(real(boxes%x + boxes%w / 2, c_double), real(boxes%y + boxes%h / 2, c_double), &
      size(data_in, 1, c_int64_t), size(data_in, 2, c_int64_t), &
      1.0_c_double, box_stats%x, box_stats%y)
   do ibox = 1, size(boxes)
      call compute_box_stats(data_in, param, boxes(ibox), box_stats(ibox)%av, box_stats(ibox)%sd)
   end do

end subroutine

subroutine estimate_gradient(buf_in, buf_out, param, err_out) bind(C)
   type(buffer_descriptor_r32_t), value :: buf_in, buf_out
   type(bkgradient_param_t), intent(in) :: param
   type(error_status_t), intent(inout) :: err_out

   real(buf_k), pointer :: data_in(:,:), data_out(:,:)
   type(box_stats_t) :: box_stats(param%n_points)
   real(buf_k) :: limit_sd, av, sd
   logical :: boxes_mask(param%n_points)
   type(bkgradient_model_t) :: model
   type(box_px_t) :: boxes(param%n_points)

   call reset_err(err_out)

   data_in => from_descriptor(buf_in)
   data_out => from_descriptor(buf_out)

   if (any(shape(data_in) /= shape(data_out))) then
      call set_err(err_out, msg="shape mismatch")
      return
   end if

   call sample_point_stats(data_in, box_sample_param_t( &
      seed=param%seed, n_points=param%n_points, &
      window_size=param%window_size, margin=param%margin), &
      boxes, box_stats, err_out)
   if (failed_err(err_out)) return

   boxes_mask(:) = .true.
   call sigclip3_r32(box_stats%sd, boxes_mask, real(param%sd_sigma, buf_k), av, sd, mode=clip_hi, fix_av=0._buf_k)
   call sigclip3_r32(box_stats%av, boxes_mask, real(param%sd_sigma, buf_k), av, sd, mode=clip_hi)

#  ifndef NDEBUG
   block
      integer i
      write (*, *) " Points used for background estimation:"
      do i = 1, param%n_points
         write (*, '(i5,2f11.2,2e11.3,2x,a)') i, box_stats(i)%x, box_stats(i)%y, box_stats(i)%av, box_stats(i)%sd, &
            merge(' ', 'X', boxes_mask(i))
      end do
   end block
#  endif

   if (count(boxes_mask) < 3) then
      call set_err(err_out, msg="all points for background estimations were rejected")
      return
   end if

   call linfit2d(pack(real(box_stats%x, buf_k), boxes_mask), pack(real(box_stats%y, buf_k), boxes_mask),&
      pack(box_stats%av - real(param%sigma_shift, buf_k) * box_stats%sd, boxes_mask), model%a, model%b, model%c, err_out)

   if (failed_err(err_out)) return

   call generate_background(data_out, param, model)

   if (param%visualize) then
      call draw_boxes(data_out, pack(boxes, boxes_mask))
   end if

end subroutine

subroutine generate_boxes(dims, param, boxes)
   integer(c_int64_t) :: dims(2)
   type(box_sample_param_t) :: param
   type(box_px_t) :: boxes(:)
   integer(c_int64_t) :: ibox
   real(c_double) :: cx_fl, cy_fl
   integer :: state

   ibox = 1
   state = param%seed

   do while (ibox <= size(boxes, kind=c_int64_t))

      cx_fl = next_random(state)
      cy_fl = next_random(state)

      boxes(ibox) % x = cx_fl * (dims(1) - 2 * param%margin - param%window_size) + param%margin
      boxes(ibox) % y = cy_fl * (dims(2) - 2 * param%margin - param%window_size) + param%margin
      boxes(ibox) % w = param%window_size
      boxes(ibox) % h = param%window_size

      ibox = ibox + 1
   end do
end subroutine

subroutine compute_box_stats(data_in, param, box, box_av, box_sd)
   real(buf_k) :: data_in(:,:), box_av, box_sd
   type(box_sample_param_t) :: param
   type(box_px_t) :: box

   call avsd(data_in(box%x:box%x + box%w - 1, box%y:box%y+box%w - 1), box_av, box_sd)

end subroutine

subroutine generate_background(data_out, param, model)
   real(buf_k) :: data_out(:,:)
   type(bkgradient_model_t) :: model
   type(bkgradient_param_t) :: param

   integer(c_int64_t) :: i, j
   real(c_double) :: x, y

   do concurrent (i = 1:size(data_out, 1, c_int64_t), j = 1:size(data_out, 2, c_int64_t))
      call ij_to_xy(real(i, c_double), real(j, c_double), &
         size(data_out, 1, c_int64_t), size(data_out, 2, c_int64_t), 1.0_c_double, &
         x, y)
      data_out(i, j) = (model%a * x + model%b * y) * param%gradient + model%c * param%offset
   end do
end subroutine

subroutine draw_boxes(data_out, boxes)
   use ieee_arithmetic
   real(buf_k) :: data_out(:,:)
   type(box_px_t) :: boxes(:)
   real(buf_k) :: nan
   integer :: ibox, ix, iy

   nan = ieee_value(1._buf_k, ieee_quiet_nan)


   do ibox = 1, size(boxes)
      associate (box =>boxes(ibox))
         do ix = box%x, box%x + box%w
            data_out(ix, box%y) = nan
            data_out(ix, box%y+box%h) = nan
         end do
         do iy = box%y, box%y + box%h
            data_out(box%x, iy) = nan
            data_out(box%x + box%w, iy) = nan
         end do
      end associate
   end do

end subroutine

end module
