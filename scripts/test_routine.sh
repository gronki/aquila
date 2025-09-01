#!/usr/bin/env bash
set -e

rm -f *.fits *.png
which aqstack
which aqlrgb
which aq2

set -x

aqstack bias testdata/m94/Bias/*

test -f bias.fits

aqstack flat -bias bias.fits testdata/m94/Flat/L/good_Flat_L_0.015_secs_0{01..08}.fits -o flat_L.fits

test -f flat_L.fits

aqstack dark testdata/m94/Dark/good_Dark_100_secs_0{01..03}.fits -bias bias.fits

test -f dark.fits

aqstack final -bias bias.fits -dark dark.fits -hot-only -flat flat_L.fits \
    -ref testdata/m94/Light/L/m94_Light_L_600_secs_001.fits \
    -align gravity -o stack_L.fits \
    testdata/m94/Light/L/m94_Light_L_600_secs_0{01..02}.fits

test -f stack_L.fits

aqlrgb stack_L.fits testdata/m94/stack_{R,G,B}.fits -best -o lrgb.png

test -f lrgb.fits
test -f lrgb.png

aq2 <examples/script/sho.aq2

test -f sho_r.fits
test -f sho.png

aq2 <examples/script/stack.aq2

test -f bias_new.fits
test -f stack_new.fits

echo OK