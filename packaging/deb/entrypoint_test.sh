set -e

PACKAGE_NAME="aquila-${VERSION:?}-${DISTRO:?}-${ARCH:?}"

cd /packaging
apt update
apt install -y ./"$PACKAGE_NAME".deb

cd $(mktemp -d)
cp -r /testdata/* .

aqlrgb m42_{R,G,B}.fits -best -o test.fits
test -f test.fits
cd 250428
aqstack bias Bias/good*
test -f bias.fits
aqstack flat Flat/L/*
test -f flat.fits
aqstack dark Dark/* -bias bias.fits
test -f dark.fits
aqstack final -bias bias.fits -dark dark.fits -hot-only -flat flat.fits \
    Light/L/m94_Light_L_600* -o stack.fits
test -f stack.fits

echo OK