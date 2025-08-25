#!/usr/bin/env bash
set -e
set +x

mkdir -p testdata
rm -rf testdata/*
cd testdata

curl -L https://github.com/gronki/aquila_test_data/raw/refs/heads/master/testdata.tar.gz -o testdata.tar.gz
tar xzfv testdata.tar.gz
rm -f testdata.tar.gz