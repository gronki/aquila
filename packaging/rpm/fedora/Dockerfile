FROM fedora:latest AS builder
RUN dnf install -y gcc-gfortran fedora-packager rpmdevtools gdb cfitsio-devel libpng-devel

WORKDIR /fpm
ADD https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 fpm.F90
RUN gfortran -O0 fpm.F90 -o fpm && install fpm /usr/local/bin/

ARG MY_UID=1000
ARG MY_GID=1000
ARG USERNAME=user
RUN groupadd -g ${MY_GID} "${USERNAME}" && useradd -u ${MY_UID} -g ${MY_GID} "${USERNAME}"
USER "${USERNAME}"

RUN echo "%_topdir /home/${USERNAME}/build/rpmbuild" >> "/home/${USERNAME}/.rpmmacros"

ENV VERSION="unknown"
ENV FPM_FC=gfortran

WORKDIR /source
ENTRYPOINT [ "bash", "packaging/rpm/entrypoint.sh", "/result" ]