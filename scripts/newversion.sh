#!/usr/bin/env bash
set -e

if [ -n "$(git status --porcelain)" ]; then
    echo "git tree is dirty; commit all changes before the version bump"
    exit 1
fi

if [ -z "$1" ]; then
VERSION_STR=$(date -d '-3hours' +'%y%m%d')
else
VERSION_STR=$1
fi


if [ -n "$(git tag -l \"$VERSION_STR\")" ]; then
    echo "Tag $VERSION_STR already exists."
    exit 1
fi

if [ ! -f fpm.toml ]; then
    echo "this script must be run from repo root directory"
    exit 1
fi

sed -Ei "s/version =.*$/version = '${VERSION_STR}'/" src/globals/globals.F90
sed -Ei "s/version =.*$/version = \"${VERSION_STR}\"/" fpm.toml

if [ -z "$(git status --porcelain)" ]; then
    echo "git does not see any changes; probably version $VERSION_STR already exists:"
    echo
    head -n 5 fpm.toml
    echo
    grep version src/globals/globals.F90 | head -n 1
    exit 1
fi

git add src/globals/globals.F90 fpm.toml
git commit -m "Version $VERSION_STR"
git tag "$VERSION_STR"

git push
git push --tags