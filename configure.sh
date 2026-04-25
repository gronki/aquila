#!/usr/bin/env bash

REPO_ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

BT=${1:-release}
BD=${2:-"$REPO_ROOT/build"}

cmake -B "$BD" -S "$REPO_ROOT" \
	-DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE="$BT"
