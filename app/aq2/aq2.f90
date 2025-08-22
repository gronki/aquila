program test_console

   use error_m
   use namespace_m
   use operation_database_m
   use example_operations_m
   use runner_m
   use readline_prompt_m
   use OPERATION_IMPORTS_M
   use globals, only: greeting, cf

   type(namespace_t), target :: namespace
   type(operation_db_t) :: operation_db
   type(err_t) :: err

   call greeting(cf("aq", "1") // cf('2','1;36') // " script")
   call operation_db_init(operation_db)
   call init_operations(operation_db)
   call run_interactive_console(readline_prompt_t(), operation_db, namespace, err)
   if (check(err)) error stop

end program

