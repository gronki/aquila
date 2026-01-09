#!/usr/bin/env bash

[ -f scripts/install.sh ] || exit 1

set -e

mkdir -p build
rm -rf build/*
cd build
cmake .. -DCMAKE_BUILD_TYPE=Native -DOPENMP=On -DCMAKE_INSTALL_PREFIX=/opt/aquila
cmake --build . -j $(nproc) --verbose
ctest . --output-on-failure
sudo cmake --install .

