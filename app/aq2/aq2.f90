program test_console

    use tokenizer_m
    use parser_m
    use command_m
    use namespace_m
    use operation_database_m
    use example_operations_m
    use line_error_m

    character(len=4096) :: line
    type(ast_statement_t) :: stmt
    type(namespace_t) :: ns
    type(value_item_t) :: retval
    type(err_t) :: err
    type(tok_array_t) :: tokens
    type(operation_db_t) :: operation_db

    operation_db = get_example_operation_db()

    do
        write (*, '(a)', advance='no') '> '
        read (*, '(a)') line
        if (adjustl(line) == "exit") exit
        if (line == "") cycle

        call tokenize_into_array(trim(line), tokens)
        call parse_statement(tokens, stmt, err)

        if (check(err)) then
            write(*, '(dt)') error_with_line(err, line)
            cycle
        end if 

        call execute_statement(stmt, retval%value, ns, operation_db, err)

        if (check(err)) then
            write(*, '(dt)') error_with_line(err, line)
            cycle
        end if 

        associate (val=>retval%value)
            print *, "RESULT ::::::::::::::: ", val%get_trace(), " = ", val%to_str()
        end associate

    end do
end program
