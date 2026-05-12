[ -f scripts/rundebug.sh ] || exit 1

mkdir -p build

(
    set -e
    cd build
    cmake .. -DCMAKE_BUILD_TYPE=debug -DAQUILA_OPENMP=on
    cmake --build .
    ctest .
    ldd aq2
    ./aq2
)