#!/usr/bin/env bash

[ -f scripts/install.sh ] || exit 1

set -e

PROJ_DIR=$(pwd)

(
    cd $(mktemp -d)
    cmake "$PROJ_DIR" -DCMAKE_BUILD_TYPE=Native -DAQUILA_OPENMP=On -DCMAKE_INSTALL_PREFIX=/opt/aquila
    cmake --build . --verbose
    ctest . --output-on-failure
    sudo cmake --install .
)

