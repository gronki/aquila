#!/usr/bin/env bash

# this script will build and test packages for all
# supported Linux distributions that prefer
# .deb packaging format (Ubuntu, Debian).

set -e

for distro in ubuntu-{noble-intel,noble,jammy-intel,focal-intel}; do
    echo " *** BUILDING IMAGE FOR ${distro} ***"
    bash packaging/deb/build_image.sh ${distro} 
    echo
    echo " *** BUILDING THE PACKAGE FOR ${distro} ***"
    if ! bash packaging/deb/build_package.sh ${distro}; then
        echo "DANGER: build for $distro failed"
        continue
    fi
    echo
    echo " *** TESTING THE PACKAGE FOR ${distro} ***"
    if ! bash packaging/deb/test_package.sh ${distro}; then 
        echo "DANGER: test for $distro failed"
    fi
    echo
    echo
done