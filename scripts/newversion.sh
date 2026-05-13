#!/usr/bin/env bash
set -e

if [ -z "$1" ]; then
VERSION_STR=$(date -d '-3hours' +'%y%m%d')
else
VERSION_STR=$1
fi

if [ -z "$2" ]; then
ANNOTATION="v$VERSION_STR"
else
ANNOTATION="v$VERSION_STR: $2"
fi

if [ -n "$(git tag -l \"$VERSION_STR\")" ]; then
    echo "Tag $VERSION_STR already exists."
    exit 1
fi

if [ ! -f scripts/newversion.sh ]; then
    echo "this script must be run from repo root directory"
    exit 1
fi

sed -i "s/define AQUILA_VERSION .*$/define AQUILA_VERSION \"${VERSION_STR}\"/" src/c_binding/aquila.h
sed -i "s/version = .*$/version = \"${VERSION_STR}\"/" src/globals/globals.F90
sed -i "s/aquila VERSION \".*\"/aquila VERSION \"${VERSION_STR}\"/" CMakeLists.txt
sed -i "s/^version:.*$/version: \"${VERSION_STR}\"/" snap/snapcraft.yaml

(
    git add .
    git commit -m "Version $VERSION_STR"
) || echo "no changes to add"

git tag --delete "$VERSION_STR" || echo "Tag does not exist. creating..."
git push --delete origin "$VERSION_STR" || echo "No remote tag."

git tag -a "$VERSION_STR" -m "$ANNOTATION"
git push
git push origin "$VERSION_STR"