set -e -x

function hyperclean {
    fpm clean -all
    ( cd deps/task-dispatch && fpm clean -all )
    ( cd deps/fastconv && fpm clean -all )

    find -name \*.mod -print -delete
    find -name \*.smod -print -delete
    find -name \*.o -print -delete
    rm -rf testbuild
}

[ ! -d testdata ] && scripts/get_test_data.sh

export PATH="$(realpath testbuild/bin):$PATH"
hyperclean && fpm @test-ifx-debug && scripts/test_routine.sh
hyperclean && fpm @test-ifx-release && scripts/test_routine.sh
hyperclean && fpm @test-gfortran-debug && scripts/test_routine.sh
hyperclean && fpm @test-gfortran-release && scripts/test_routine.sh