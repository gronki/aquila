#!/usr/bin/env bash

set -e
set +x

PACKAGE_NAME=$1

echo "testing ${PACKAGE_NAME}"

cd /source/packaging
apt-get update
apt-get install -y ./"$PACKAGE_NAME"

mkdir -p /tmp/test
cd /tmp/test

ln -s /source/aquila_test_data testdata
ln -s /source/examples

bash /source/scripts/test_routine.sh

echo OK