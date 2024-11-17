#!/usr/bin/env bash

set -e

DISTRO=$1

IMAGE_NAME="aquila_package_builder:${DISTRO}-latest"
DOCKERFILE_PATH="packaging/rpm/${DISTRO}/Dockerfile"

if [ ! -f "$DOCKERFILE_PATH" ]; then
    echo "usage: packaging/rpm/build_package.sh [distro_name]"
    exit 1
fi

docker build --pull -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" --build-arg MY_UID=$(id -u) --build-arg MY_GID=$(id -g) .
bash packaging/sourceball.sh
mkdir -p "$(pwd)/packaging/rpm/result"

docker run -it --rm \
    -u $(id -u):$(id -g) \
    -e DISTRO="${DISTRO:?}" \
    -e VERSION=$(grep 'parameter :: version' src/globals.F90 | cut -d\' -f2) \
    -v "$(pwd):/source" \
    -v "$(pwd)/packaging/rpm/result:/rpmbuild/RPMS" \
    --workdir /source \
    "$IMAGE_NAME"