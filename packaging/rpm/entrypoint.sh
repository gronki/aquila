set -ex

NAMEVERSION="aquila-${VERSION}"

cd $(rpm -E %_topdir)
cp /source/packaging/src/*.tar.gz SOURCES/

cp /source/packaging/rpm/aquila.spec SPECS/${NAMEVERSION}.spec
sed -i "s/^Version:.*$/Version: ${VERSION:?}/" SPECS/${NAMEVERSION}.spec
sed -i "s/^Requires:.*$/Requires: ${REQUIRES:?}/" SPECS/${NAMEVERSION}.spec
sed -i "s/^BuildRequires:.*$/BuildRequires: ${BUILD_REQUIRES:?}/" SPECS/${NAMEVERSION}.spec

rpmbuild -ba SPECS/${NAMEVERSION}.spec
