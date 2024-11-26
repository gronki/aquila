#!/usr/bin/env bash

set -e

CONFIG_PATH="packaging/rpm/configs/${1}.sh"

if [ ! -f "$CONFIG_PATH" ]; then
    echo "usage: packaging/rpm/build_package.sh [distro_name]"
    exit 1
fi

source "$CONFIG_PATH"

docker build --pull -t "$IMAGE_NAME" -f "$DOCKERFILE_PATH" \
    --build-arg MY_UID=$(id -u) \
    --build-arg MY_GID=$(id -g)  \
    --build-arg ARG_IMAGE_NAME="$IMAGE_NAME"  \
    --build-arg ARG_IMAGE_TAG="$IMAGE_TAG"  \
    --build-arg ARG_FC="$FC"  \
    --build-arg ARG_REQUIRES="$REQUIRES"  \
    --build-arg ARG_BUILD_REQUIRES="$BUILD_REQUIRES"  \
    .
bash packaging/sourceball.sh
mkdir -p "$(pwd)/packaging/rpm/result"

docker run -it --rm \
    -u $(id -u):$(id -g) \
    -e VERSION=$(grep 'parameter :: version' src/globals.F90 | cut -d\' -f2) \
    -e LIBPNG_NAME \
    -v "$(pwd):/source" \
    -v "$(pwd)/packaging/rpm/result:/rpmbuild/RPMS" \
    --workdir /source \
    "$IMAGE_NAME"