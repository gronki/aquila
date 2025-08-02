set -e

PACKAGE_NAME=$1

echo "testing ${PACKAGE_NAME}"

cd /packaging
apt update
apt install -y ./"$PACKAGE_NAME"

cd $(mktemp -d)
cp -r /testdata/* .

aqlrgb m42_{R,G,B}.fits -best -o test.fits
test -f test.fits

aqstack bias 250428/Bias/good*
test -f bias.fits
aqstack flat 250428/Flat/L/*
test -f flat.fits
aqstack dark 250428/Dark/* -bias bias.fits
test -f dark.fits
aqstack final -bias bias.fits -dark dark.fits -hot-only -flat flat.fits \
    -align gravity 250428/Light/L/m94_Light_L_600* -o stack.fits
test -f stack.fits

aq2 <test.aq2
echo OK