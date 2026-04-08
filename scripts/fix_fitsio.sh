#!/usr/bin/env bash
set -e
sed -i -e 's/${LIB_NAME}/${FITSIO_STATIC}/g' -e 's/ cfitsio / ${FITSIO_STATIC} /g'  -e 's/cfitsio)/${FITSIO_STATIC})/g' \
    CMakeLists.txt