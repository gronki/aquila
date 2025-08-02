#!/usr/bin/env bash

set -e

DISTRO=$1

IMAGE_NAME="aquila_package_builder:${DISTRO}"
DOCKERFILE_DIR="packaging/deb/${DISTRO}"
DOCKERFILE_PATH="${DOCKERFILE_DIR}/Dockerfile"
DOCKER=podman
ARCH=amd64

if [ ! -f "$DOCKERFILE_PATH" ]; then
    echo "usage: packaging/deb/build_package.sh [distro_name]"
    echo "(required to run from root directory of the repo)"
    exit 1
fi

if ! ${DOCKER} image inspect ${IMAGE_NAME} >/dev/null; then
    echo "image $IMAGE_NAME not found; please first run"
    echo "packaging/deb/build_image.sh ${DISTRO}"
    exit 1
fi

fpm clean -all
find -name '*.mod' -delete
find -name '*.smod' -delete

VERSION=$(grep 'version =' src/globals/globals.F90 | cut -d\' -f2)

${DOCKER} run -it --rm \
    -e DISTRO="$DISTRO" \
    -e VERSION="$VERSION" \
    -e ARCH="$ARCH" \
    -v $(pwd):/source \
    -v $(mktemp -d):/source/build \
    -v $(pwd)/packaging:/result \
    "$IMAGE_NAME"