#!/usr/bin/env bash

set -e

DISTRO=$1

TEST_IMAGE_NAME="aquila_package_tester:${DISTRO}"
DOCKERFILE_DIR="packaging/deb/${DISTRO}"
TEST_DOCKERFILE_PATH="${DOCKERFILE_DIR}/Dockerfile.test"
DOCKER=podman
ARCH=amd64

if [ ! -f "$TEST_DOCKERFILE_PATH" ]; then
    echo "usage: packaging/deb/build_package.sh [distro_name]"
    echo "(required to run from root directory of the repo)"
    exit 1
fi

VERSION=$(grep 'version =' src/globals/globals.F90 | cut -d\' -f2)
PACKAGE_NAME="aquila-${VERSION:?}-${DISTRO:?}-${ARCH:?}.deb"

if ! ${DOCKER} image inspect ${TEST_IMAGE_NAME} >/dev/null; then
    echo "image $TEST_IMAGE_NAME not found; please first run"
    echo "packaging/deb/build_image.sh ${DISTRO}"
    exit 1
fi

TESTDIR=$(mktemp -d)
echo "testdir = $TESTDIR"

${DOCKER} run -it --rm \
    -v $(pwd):/source \
    -v $TESTDIR:/tmp/test \
    --entrypoint "/bin/bash" \
    "$TEST_IMAGE_NAME" /source/packaging/deb/entrypoint_test.sh "$PACKAGE_NAME" || (
        mkdir -p packaging/failed
        # mv "packaging/${PACKAGE_NAME}" packaging/failed/
    )
