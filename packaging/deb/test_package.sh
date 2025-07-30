#!/usr/bin/env bash

set -e

DISTRO=$1

IMAGE_NAME="aquila_package_tester:${DISTRO}"
DOCKERFILE_DIR="packaging/deb/${DISTRO}"
DOCKERFILE_PATH="${DOCKERFILE_DIR}/Dockerfile.test"
DOCKER=podman

if [ ! -f "$DOCKERFILE_PATH" ]; then
    echo "usage: packaging/deb/build_package.sh [distro_name]"
    echo "(required to run from root directory of the repo)"
    exit 1
fi

VERSION=$(grep 'version =' src/globals/globals.F90 | cut -d\' -f2)

if ! ${DOCKER} image inspect ${IMAGE_NAME} >/dev/null; then
    ${DOCKER} build -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .
fi

${DOCKER} run -it --rm \
    -e DISTRO="${DISTRO:?}" \
    -e VERSION="$VERSION" \
    -e ARCH="x86_64" \
    -v $(pwd)/packaging:/packaging \
    -v $(pwd)/scratch:/testdata \
    --entrypoint "/bin/bash" \
    "$IMAGE_NAME" /packaging/deb/entrypoint_test.sh