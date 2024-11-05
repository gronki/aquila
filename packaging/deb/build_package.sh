#!/usr/bin/env bash

set -e

DISTRO=$1

IMAGE_NAME="aquila_package_builder:${DISTRO}-latest"
DOCKERFILE_PATH="packaging/deb/${DISTRO}/Dockerfile"

if [ ! -f "$DOCKERFILE_PATH" ]; then
    echo "usage: packaging/deb/build_package.sh [distro_name]"
    exit 1
fi

if [ -z "$(docker images -q "$IMAGE_NAME"  2> /dev/null)" ]; then
    docker build --pull -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .
fi

FFLAGS="-O3 -g1 -funsafe-math-optimizations -fopenmp"
if [ $(arch) == x86_64 ]; then
    FFLAGS="$FFLAGS -mavx2"
fi

docker run -it --rm \
    -u $(id -u):$(id -g) \
    -e DISTRO="${DISTRO:?}" \
    -e FPM_FFLAGS="$FFLAGS" \
    -e VERSION=$(grep 'parameter :: version' src/globals.F90 | cut -d\' -f2) \
    -v $(pwd):/source \
    -v $(mktemp -d):/source/build \
    -v $(pwd)/packaging:/result \
    --workdir /source \
    "$IMAGE_NAME"