set -e
DISTRO=${1:?}
test -f "packaging/deb/${DISTRO:?}/Dockerfile"
docker build -t aquila_debian_package_builder -f packaging/deb/${DISTRO:?}/Dockerfile .
docker run -it --rm -u $(id -u):$(id -g) -e DISTRO="${DISTRO:?}" -v $(pwd):/source -v $(pwd)/packaging:/result aquila_debian_package_builder