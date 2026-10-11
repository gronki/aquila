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

type, bind(C) :: bkgradient_param_t
   real(c_double) :: sd_sigma = 2.5, offset = 1.0
   integer(c_int64_t) :: start_x = 55, start_y = 67, n_points = 64, window_size = 16, margin = 32
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

subroutine estimate_gradient(buf_in, buf_out, param, err_out) bind(C)
   type(buffer_descriptor_r32_t), value :: buf_in, buf_out
   type(bkgradient_param_t), intent(in) :: param
   type(error_status_t), intent(inout) :: err_out

   real(buf_k), pointer :: data_in(:,:), data_out(:,:)
   type(box_px_t) :: boxes(param%n_points)
   real(buf_k) :: boxes_av(param%n_points), boxes_sd(param%n_points)
   real(c_double) :: boxes_x(param%n_points), boxes_y(param%n_points)
   real(buf_k) :: limit_sd
   logical(c_bool) :: boxes_mask(param%n_points)
   integer :: ibox
   type(bkgradient_model_t) :: model

   call reset_err(err_out)

   data_in => from_descriptor(buf_in)
   data_out => from_descriptor(buf_out)

   if (any(shape(data_in) /= shape(data_out))) then
      call set_err(err_out, msg="shape mismatch")
      return
   end if

   call generate_boxes(shape(data_in, kind=c_int64_t), param, boxes)
   do ibox = 1, size(boxes)
      call compute_box_stats(data_in, param, boxes(ibox), boxes_av(ibox), boxes_sd(ibox))
   end do
   limit_sd = sigclip2_fixed(boxes_sd, real(param%sd_sigma, buf_k), 0._buf_k)
   if (.not.ieee_is_normal(limit_sd)) then
      call set_err(err_out, msg="all points for background estimations were rejected")
      return
   end if
   boxes_mask = boxes_sd < param%sd_sigma * limit_sd

   if (count(boxes_mask) < 3) then
      call set_err(err_out, msg="all points for background estimations were rejected")
      return
   end if

   call ij_to_xy(real(boxes%x + boxes%w / 2, c_double), real(boxes%y + boxes%h / 2, c_double), &
      size(data_in, 1, c_int64_t), size(data_in, 2, c_int64_t), &
      1.0_c_double, boxes_x, boxes_y)

   call linfit2d(pack(real(boxes_x, buf_k), boxes_mask), pack(real(boxes_y, buf_k), boxes_mask),&
      pack(boxes_av, boxes_mask), model%a, model%b, model%c, err_out)

   if (failed_err(err_out)) return

   model%c = model%c * param%offset

   call generate_background(data_out, model)

end subroutine

subroutine generate_boxes(dims, param, boxes)
   integer(c_int64_t) :: dims(2)
   type(bkgradient_param_t) :: param
   type(box_px_t) :: boxes(:)
   integer(c_int64_t) :: ibox, counter_x, counter_y
   real(c_double) :: cx_fl, cy_fl

   ibox = 1
   counter_x = param%start_x-1
   counter_y = param%start_y-1

   do while (ibox <= size(boxes, kind=c_int64_t))
      counter_x = counter_x + 1
      counter_y = counter_y + 1

      cx_fl = modulo(golden * counter_x, 1.0_c_double)
      cy_fl = modulo(golden * counter_y, 1.0_c_double)

      boxes(ibox) % x = cx_fl * (dims(1) - 2 * param%margin - param%window_size) + param%margin
      boxes(ibox) % y = cy_fl * (dims(2) - 2 * param%margin - param%window_size) + param%margin
      boxes(ibox) % w = param%window_size
      boxes(ibox) % h = param%window_size

      ibox = ibox + 1
   end do
end subroutine

subroutine compute_box_stats(data_in, param, box, box_av, box_sd)
   real(buf_k) :: data_in(:,:), box_av, box_sd
   type(bkgradient_param_t) :: param
   type(box_px_t) :: box

   call avsd(data_in(box%x:box%x + box%w - 1, box%y:box%y+box%w - 1), box_av, box_sd)

end subroutine

subroutine generate_background(data_out, model)
   real(buf_k) :: data_out(:,:)
   type(bkgradient_model_t) :: model

   integer(c_int64_t) :: i, j
   real(c_double) :: x, y

   do concurrent (i = 1:size(data_out, 1, c_int64_t), j = 1:size(data_out, 2, c_int64_t))
      call ij_to_xy(real(i, c_double), real(j, c_double), &
         size(data_out, 1, c_int64_t), size(data_out, 2, c_int64_t), 1.0_c_double, &
         x, y)
      data_out(i, j) = model%a * x + model%b * y + model%c
   end do
end subroutine

end module
