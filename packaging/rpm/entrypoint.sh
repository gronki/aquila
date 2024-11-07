set -e

NAMEVERSION="aquila-${VERSION}"

cp -r /source/ /tmp/${NAMEVERSION}/
cd /tmp
tar czfv ${NAMEVERSION}.tar.gz --exclude=.git --exclude=build --exclude=packaging ${NAMEVERSION}
rm -rf ${NAMEVERSION}

rpmdev-setuptree
cd $(rpm -E %_topdir)
mv /tmp/${NAMEVERSION}.tar.gz SOURCES/

cp /source/packaging/rpm/aquila.spec SPECS/${NAMEVERSION}.spec
sed -i "s/^Version:.*$/Version: ${VERSION:?}/" SPECS/${NAMEVERSION}.spec

rpmbuild -ba SPECS/${NAMEVERSION}.spec
