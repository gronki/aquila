#!/usr/bin/env bash

set -e

DISTRO=$1

IMAGE_NAME="aquila_package_builder:${DISTRO}"
DOCKERFILE_DIR="packaging/deb/${DISTRO}"
DOCKERFILE_PATH="${DOCKERFILE_DIR}/Dockerfile"
TEST_IMAGE_NAME="aquila_package_tester:${DISTRO}"
TEST_DOCKERFILE_PATH="${DOCKERFILE_DIR}/Dockerfile.test"
DOCKER=podman

if [ ! -f "$DOCKERFILE_PATH" ]; then
    echo "usage: packaging/deb/build_image.sh [distro_name]"
    echo "(required to run from root directory of the repo)"
    exit 1
fi

[ ! -f packaging/fpm.F90 ] && 
    curl -L \
    https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0.F90 \
    -o packaging/fpm.F90

${DOCKER} build -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .
${DOCKER} build -t "$TEST_IMAGE_NAME" -f "$TEST_DOCKERFILE_PATH" .