#!/usr/bin/env bash

if [ -z "$1" ]; then
VERSION_STR=$(date -d '-3hours' +'%y%m%d')
else
VERSION_STR=$1
fi

if [ ! -f fpm.toml ]; then
    echo "this script must be run from repo root directory"
    exit 1
fi

sed -Ei "s/version =.*$/version = '${VERSION_STR}'/" src/globals/globals.F90
sed -Ei "s/version =.*$/version = \"${VERSION_STR}\"/" fpm.toml