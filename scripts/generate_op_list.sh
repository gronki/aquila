#!/usr/bin/env bash

# [ -f fpm.toml ] || { echo "this script must be run from repo root dir"; exit 1 }

MODULE_SOURCE="app/aq2/operation_imports.f90"


cat /dev/null > ${MODULE_SOURCE}

echo "module operation_imports_m" >> ${MODULE_SOURCE}
echo "use operation_database_m, only: operation_db_t, add_operation" >> ${MODULE_SOURCE}

function find_operations {
    grep -REih 'extends\s*\(\s*operation_t\s*\)' src | cut -d: -f3 | awk '{ print $1 }' | sort 
}

find_operations | while read opname; do
    echo "use ${opname%_t}_m, only: ${opname}" >> ${MODULE_SOURCE}
done

echo "implicit none (type, external)" >> ${MODULE_SOURCE}
echo "contains" >> ${MODULE_SOURCE}
echo "subroutine init_operations(db)" >> ${MODULE_SOURCE}
echo "type(operation_db_t), intent(inout) :: db" >> ${MODULE_SOURCE}


find_operations | while read opname; do
    echo "call add_operation(db, ${opname}())" >> ${MODULE_SOURCE}
done

echo "end subroutine" >> ${MODULE_SOURCE}
echo "end module" >> ${MODULE_SOURCE}