module operation_imports_m
use operation_database_m, only: operation_db_t, add_operation
use add_op_m, only: add_op_t
use align_op_m, only: align_op_t
use asinh_op_m, only: asinh_op_t
use calibrate_op_m, only: calibrate_op_t
use convol_op_m, only: convol_op_t
use deconv_op_m, only: deconv_op_t
use findstar_op_m, only: findstar_op_t
use kernel_op_m, only: kernel_op_t
use load_legacy_frame_op_m, only: load_legacy_frame_op_t
use lrgb_op_m, only: lrgb_op_t
use mix_op_m, only: mix_op_t
use png_op_m, only: png_op_t
use project_op_m, only: project_op_t
use stack_op_m, only: stack_op_t
use stretch_op_m, only: stretch_op_t
use write_op_m, only: write_op_t
implicit none (type, external)
contains
subroutine init_operations(db)
type(operation_db_t), intent(inout) :: db
call add_operation(db, add_op_t())
call add_operation(db, align_op_t())
call add_operation(db, asinh_op_t())
call add_operation(db, calibrate_op_t())
call add_operation(db, convol_op_t())
call add_operation(db, deconv_op_t())
call add_operation(db, findstar_op_t())
call add_operation(db, kernel_op_t())
call add_operation(db, load_legacy_frame_op_t())
call add_operation(db, lrgb_op_t())
call add_operation(db, mix_op_t())
call add_operation(db, png_op_t())
call add_operation(db, project_op_t())
call add_operation(db, stack_op_t())
call add_operation(db, stretch_op_t())
call add_operation(db, write_op_t())
end subroutine
end module
