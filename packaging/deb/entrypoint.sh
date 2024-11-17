set -ex

PACKAGE_CREATE_DIR=$(mktemp -d)
PACKAGE_RESULT_DIR="/result/${DISTRO}/${DISTRO_VERSION}/"
NAMEVERSION="aquila-${VERSION}"

echo "building version ${VERSION} for ${DISTRO} ${DISTRO_VERSION} using ${FPM_FC} and dependencies ${DEP_PACKAGES}"


cd "${PACKAGE_CREATE_DIR}"
cp "/source/packaging/src/aquila-${VERSION}.tar.gz" "aquila_${VERSION}.orig.tar.gz"
tar xzfv "aquila_${VERSION}.orig.tar.gz"
cd "$NAMEVERSION"
cp -r /source/packaging/deb/debian .
sed -i "s/^Depends:.*$/Depends: ${DEP_PACKAGES:?}/" debian/control
sed -i "s/^Build-Depends:.*$/Build-Depends: debhelper-compat (= 12), gfortran-${GCC_VERSION}, libcfitsio-dev, libpng-dev/" debian/control
dpkg-buildpackage -us -uc
mkdir -p "${PACKAGE_RESULT_DIR}"
cp ../*.deb "${PACKAGE_RESULT_DIR}"
