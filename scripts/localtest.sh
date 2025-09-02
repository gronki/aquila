#!/usr/bin/env bash
set -e -x

[ -d deps ] || exit 1

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

hyperclean

fpm @test-${1:-ifx-debug}
CODE_PATH=$(realpath .)
export PATH="$CODE_PATH/testbuild/bin:$PATH"

cd $(mktemp -d)
pwd
ln -s "$CODE_PATH/examples"
ln -s "$CODE_PATH/testdata"
bash "${CODE_PATH}/scripts/test_routine.sh"