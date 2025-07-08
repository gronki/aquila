module operation_imports_m
use operation_database_m, only: operation_db_t, add_operation
use findstar_op_m, only: findstar_op_t
use load_legacy_frame_op_m, only: load_legacy_frame_op_t
implicit none (type, external)
contains
subroutine init_operations(db)
type(operation_db_t), intent(inout) :: db
call add_operation(db, findstar_op_t())
call add_operation(db, load_legacy_frame_op_t())
end subroutine
end module
