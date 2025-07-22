#!/usr/bin/env bash

set -e

DISTRO=$1

IMAGE_NAME="aquila_package_builder:${DISTRO}"
DOCKERFILE_DIR="packaging/deb/${DISTRO}"
DOCKERFILE_PATH="${DOCKERFILE_DIR}/Dockerfile"
DOCKER=podman

if [ ! -f "$DOCKERFILE_PATH" ]; then
    echo "usage: packaging/deb/build_package.sh [distro_name]"
    echo "(required to run from root directory of the repo)"
    exit 1
fi

[ ! -f packaging/fpm.F90 ] && 
    curl -L \
    https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 \
    -o packaging/fpm.F90

VERSION=$(grep 'version =' src/globals/globals.F90 | cut -d\' -f2)

# if ! ${DOCKER} images -aq ${IMAGE_NAME}; then
    ${DOCKER} build -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .
# fi

${DOCKER} run -it --rm \
    -e DISTRO="${DISTRO:?}" \
    -e VERSION="$VERSION" \
    -e ARCH="x86_64" \
    -v $(pwd):/source \
    -v $(mktemp -d):/source/build \
    -v $(pwd)/packaging:/result \
    "$IMAGE_NAME"