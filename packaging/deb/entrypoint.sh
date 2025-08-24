set -e

PACKAGE_CREATE_DIR=$(mktemp -d)
PACKAGE_RESULT_DIR=${1:?}
PACKAGE_NAME="aquila-${VERSION:?}-${DISTRO:?}-${ARCH:?}"

source /etc/buildenv.sh

rm -f "${PACKAGE_RESULT_DIR}/${PACKAGE_NAME}.deb"

mkdir -p build
rm -rf build/*
find -name \*.mod -o -name \*.smod -delete
fpm build
fpm install --prefix "$PACKAGE_CREATE_DIR"/${PACKAGE_NAME}/usr --no-rebuild
cp -r packaging/deb/DEBIAN "$PACKAGE_CREATE_DIR"/${PACKAGE_NAME}/
(
    cd "$PACKAGE_CREATE_DIR"
    sed -i "s/___VERSION___/${VERSION:?}/" ${PACKAGE_NAME}/DEBIAN/control
    sed -i "s/___DEPS___/${DEP_PACKAGES:?}/" ${PACKAGE_NAME}/DEBIAN/control
    sed -i "s/___ARCH___/${ARCH:?}/" ${PACKAGE_NAME}/DEBIAN/control
    dpkg-deb --build ${PACKAGE_NAME}
    mkdir -p "${PACKAGE_RESULT_DIR}"
    cp *.deb "${PACKAGE_RESULT_DIR}"
)
