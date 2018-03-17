module globals

    implicit none

    integer, parameter :: sp = selected_real_kind(6)
    integer, parameter :: dp = selected_real_kind(15)

    logical :: cfg_verbose = .true.

end module
