set -e

PACKAGE_CREATE_DIR=$(mktemp -d)
PACKAGE_RESULT_DIR=${1:?}
DEB_ARCH=$(dpkg --print-architecture)
PACKAGE_NAME="aquila-${VERSION:?}-${DISTRO:?}-${DEB_ARCH}"

fpm build --verbose
fpm install --prefix "$PACKAGE_CREATE_DIR"/${PACKAGE_NAME}/usr
cp -r packaging/deb/DEBIAN "$PACKAGE_CREATE_DIR"/${PACKAGE_NAME}/
(
    cd "$PACKAGE_CREATE_DIR"
    sed -i "s/^Version:.*$/Version: ${VERSION:?}/" ${PACKAGE_NAME}/DEBIAN/control
    sed -i "s/^Depends:.*$/Depends: ${DEP_PACKAGES:?}/" ${PACKAGE_NAME}/DEBIAN/control
    sed -i "s/^Architecture:.*$/Architecture: ${DEB_ARCH}/" ${PACKAGE_NAME}/DEBIAN/control
    dpkg-deb --build ${PACKAGE_NAME}
    mkdir -p "${PACKAGE_RESULT_DIR}"
    cp *.deb "${PACKAGE_RESULT_DIR}"
)
