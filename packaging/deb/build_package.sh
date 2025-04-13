#!/usr/bin/env bash

set -e

DISTRO=$1

IMAGE_NAME="aquila_package_builder:${DISTRO}-latest"
DOCKERFILE_PATH="packaging/deb/${DISTRO}/Dockerfile"
DOCKER=podman

if [ ! -f "$DOCKERFILE_PATH" ]; then
    echo "usage: packaging/deb/build_package.sh [distro_name]"
    exit 1
fi

if [ -z "$(${DOCKER} images -q ${IMAGE_NAME})" ]; then
    ${DOCKER} build -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .
fi

${DOCKER} run -it --rm \
    -e DISTRO="${DISTRO:?}" \
    -e VERSION=210111 \
    -e ARCH="x86_64" \
    -v $(pwd):/source \
    -v $(mktemp -d):/source/build \
    -v $(pwd)/packaging:/result \
    "$IMAGE_NAME"