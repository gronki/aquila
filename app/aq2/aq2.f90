program test_console

   use error_m
   use namespace_m
   use operation_database_m
   use example_operations_m
   use runner_m
   use readline_prompt_m

   type(namespace_t), target :: namespace
   type(operation_db_t) :: operation_db

   operation_db = get_example_operation_db()
   call init_operations(operation_db)

   call run_interactive_console(readline_prompt_t(), operation_db, namespace)


contains

    subroutine init_operations(db)
        use load_legacy_frame_op_m
        type(operation_db_t), intent(inout) :: db

        call add_operation(db, load_legacy_frame_op_t())
    end subroutine
end program

