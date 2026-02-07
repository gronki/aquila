#!/usr/bin/env bash
set -e

# if [ -n "$(git status --porcelain)" ]; then
#     echo "git tree is dirty; commit all changes before the version bump"
#     exit 1
# fi

if [ -z "$1" ]; then
VERSION_STR=$(date -d '-3hours' +'%y%m%d')
else
VERSION_STR=$1
fi

if [ -n "$(git tag -l \"$VERSION_STR\")" ]; then
    echo "Tag $VERSION_STR already exists."
    exit 1
fi

if [ ! -f scripts/newversion.sh ]; then
    echo "this script must be run from repo root directory"
    exit 1
fi

sed -Ei "s/define AQUILA_VERSION .*$/define AQUILA_VERSION \"${VERSION_STR}\"/" src/c_binding/aquila.h
sed -Ei "s/version = .*$/version = \"${VERSION_STR}\"/" src/globals/globals.F90
sed -Ei "s/aquila VERSION \".*\"/aquila VERSION \"${VERSION_STR}\"/" CMakeLists.txt

# if [ -z "$(git status --porcelain)" ]; then
#     echo "git does not see any changes; probably version $VERSION_STR already exists:"
#     echo
#     head -n 5 fpm.toml
#     echo
#     grep version src/globals/globals.F90 | head -n 1
#     echo "Do you want to continue? (y/n)"
#     read ANS
#     case "$ANS" in
#         [yY]) ;;
#         *) exit 1 ;;
#     esac
# fi

(
    git add .
    git commit -m "Version $VERSION_STR"
) || echo "no changes to add"

git tag --delete "$VERSION_STR" || echo "Tag does not exist. creating..."
git push --delete origin "$VERSION_STR" || echo "No remote tag."

git tag "$VERSION_STR"
git push
git push origin "$VERSION_STR"