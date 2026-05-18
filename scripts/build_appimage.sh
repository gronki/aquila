#!/usr/bin/env bash
# Builds an Aquila AppImage. Runs inside an Ubuntu 24.04 environment.
#
# Local usage (Docker):
#   docker run -it --rm -v $PWD:/src ubuntu:24.04 bash /src/scripts/build_appimage.sh
#
# Output is written to /out if that directory exists, otherwise next to the repo root.

set -euo pipefail

SRC_DIR="$(cd "$(dirname "$0")/.." && pwd)"
OUT_DIR="${OUT_DIR:-${SRC_DIR}}"
[ -d /out ] && OUT_DIR=/out

export DEBIAN_FRONTEND=noninteractive

apt-get update -y
apt-get install -y --no-install-recommends \
    cmake make gcc g++ gfortran \
    libreadline-dev pkg-config zlib1g-dev \
    wget file patchelf ca-certificates

# ── build ──────────────────────────────────────────────────────────────────
APPDIR="$(mktemp -d)/AppDir"
mkdir -p "$APPDIR" /tmp/aquila-build

cd /tmp/aquila-build
cmake "$SRC_DIR" \
    -DCMAKE_BUILD_TYPE=Release \
    -DAQUILA_OPENMP=On \
    -DAQUILA_TESTS=Off \
    -DCMAKE_INSTALL_PREFIX="$APPDIR/usr"
cmake --build . -j"$(nproc)"
cmake --install .

# ── bundle shared libraries ────────────────────────────────────────────────
mkdir -p "$APPDIR/usr/lib"
for bin in "$APPDIR/usr/bin/"*; do
    ldd "$bin" 2>/dev/null \
        | awk '/=> \// { print $3 }' \
        | grep -E 'libgfortran|libgomp|libaqtask_dispatch|libaquila|aquila2|libcfitsio|libpng16|libreadline' \
        | sort -u \
        | xargs -I{} cp -v {} "$APPDIR/usr/lib/" || true
done

# ── AppRun + .desktop ──────────────────────────────────────────────────────
cp "$SRC_DIR/scripts/AppRun"         "$APPDIR/AppRun"
cp "$SRC_DIR/snap/gui/aquila.desktop"  "$APPDIR/aquila.desktop"
sed -i 's/^Exec=.*/Exec=aquila/' "$APPDIR/aquila.desktop"
sed -i 's/^Icon=.*/Icon=aquila/' "$APPDIR/aquila.desktop"
cp "$SRC_DIR/snap/gui/aquila.png"      "$APPDIR/aquila.png"
chmod +x "$APPDIR/AppRun"

# ── package ────────────────────────────────────────────────────────────────
export ARCH=$(uname -m)
wget -q -O /tmp/appimagetool \
    https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-${ARCH}.AppImage
chmod +x /tmp/appimagetool
cd /tmp && /tmp/appimagetool --appimage-extract > /dev/null

/tmp/squashfs-root/AppRun "$APPDIR" "$OUT_DIR/aquila-${ARCH}.AppImage"

echo ""
echo "Done: $OUT_DIR/aquila-x86_64.AppImage"
