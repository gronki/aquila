#!/usr/bin/env bash

set -e

CONFIG_PATH="packaging/deb/configs/${1}.sh"

if [ ! -f "$CONFIG_PATH" ]; then
    echo "usage: packaging/deb/build_package.sh [distro_name]"
    exit 1
fi

source "$CONFIG_PATH"

IMAGE_NAME="aquila_package_builder:${DISTRO}-${DISTRO_VERSION}"

docker build --pull -t "$IMAGE_NAME" \
    -f "packaging/deb/Dockerfile" \
    --build-arg ARG_DISTRO="$DISTRO" \
    --build-arg ARG_DISTRO_VERSION="$DISTRO_VERSION" \
    --build-arg ARG_GCC_VERSION="$GCC_VERSION" \
    --build-arg ARG_DEP_PACKAGES="$DEP_PACKAGES" \
    .
    # --build-arg MY_UID=$(id -u) \
    # --build-arg MY_GID=$(id -g) \

bash packaging/sourceball.sh
mkdir -p "$(pwd)/packaging/deb/result"

docker run -it --rm \
    -u $(id -u):$(id -g) \
    -e VERSION=$(grep 'parameter :: version' src/globals.F90 | cut -d\' -f2) \
    -v "$(pwd):/source" \
    -v "$(pwd)/packaging/deb/result:/result" \
    --workdir /source \
    "$IMAGE_NAME"