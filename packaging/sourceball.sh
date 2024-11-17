#!/usr/bin/env bash

set -ex

export VERSION=$(grep 'parameter :: version' src/globals.F90 | cut -d\' -f2)
export NAMEVERSION="aquila-${VERSION}"

sed -i 's/^Version:.*$/Version:        '"${VERSION}"'/' packaging/rpm/aquila.spec
sed -i 's/^Version:.*$/Version: '"${VERSION}"'/' packaging/deb/debian/control

WORKDIR=$(mktemp -d)
cp -a . "${WORKDIR}/${NAMEVERSION}"
(
    cd "$WORKDIR"
    curl -L https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 \
        -o "${NAMEVERSION}/fpm.F90"
    tar czfv "${NAMEVERSION}.tar.gz" \
        --exclude=.git --exclude=build --exclude=packaging --exclude=\*.tar.gz \
        "${NAMEVERSION}"
)
mkdir -p packaging/src
mv "${WORKDIR}/${NAMEVERSION}.tar.gz" packaging/src/