#!/usr/bin/env bash

set -e

DISTRO=$1

IMAGE_NAME="aquila_package_builder:${DISTRO}-latest"
DOCKERFILE_PATH="packaging/deb/${DISTRO}/Dockerfile"

if [ ! -f "$DOCKERFILE_PATH" ]; then
    echo "usage: packaging/deb/build_package.sh [distro_name]"
    exit 1
fi

docker build -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" .

docker run -it --rm \
    -u $(id -u):$(id -g) \
    -e DISTRO="${DISTRO:?}" \
    -e VERSION=210111 \
    -e ARCH="x86_64" \
    -v $(pwd):/source \
    -v $(pwd)/packaging:/result \
    "$IMAGE_NAME"