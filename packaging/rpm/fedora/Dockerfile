FROM fedora:latest AS builder
RUN dnf install -y gcc-gfortran fedora-packager rpmdevtools gdb cfitsio-devel libpng-devel

RUN echo "%_topdir /rpmbuild" >> "${HOME}/.rpmmacros"
RUN rpmdev-setuptree && find /rpmbuild -type d -print | xargs chmod 777

ENV VERSION="unknown"
ENV FPM_FC=gfortran

WORKDIR /source
ENTRYPOINT [ "bash", "packaging/rpm/entrypoint.sh" ]