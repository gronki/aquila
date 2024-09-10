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
    docker build -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .
fi

FFLAGS="-O3 -g1 -funsafe-math-optimizations -fopenmp"
if [ $(arch) -eq x86_64 ]; then
    FFLAGS="$FFLAGS -mavx2"
fi

docker run -it --rm \
    -u $(id -u):$(id -g) \
    -e DISTRO="${DISTRO:?}" \
    -e FPM_FFLAGS="$FFLAGS" \
    -e VERSION=210111 \
    -e ARCH="$(arch)" \
    -v $(pwd):/source \
    -v $(pwd)/packaging:/result \
    "$IMAGE_NAME"