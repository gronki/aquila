#!/usr/bin/env bash
set -e
set +x

mkdir -p aquila_test_data
rm -rf aquila_test_data/*
cd aquila_test_data

curl -L https://github.com/gronki/aquila_test_data/raw/refs/heads/master/testdata.tar.gz -o testdata.tar.gz
tar xzfv testdata.tar.gz
rm -f testdata.tar.gz