FROM debian:bullseye AS builder
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends git gfortran-10 libcfitsio-dev libpng-dev && apt-get clean

WORKDIR /fpm
ADD https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 fpm.F90
RUN gfortran-10 -O0 fpm.F90 -o fpm && install fpm /usr/local/bin/

WORKDIR /source

ENV VERSION="unknown"
ENV DEP_PACKAGES="libcfitsio9, libpng16-16, libgomp1, libgfortran5"
ENV FPM_FC=gfortran-10

RUN mkdir -p /result

ENTRYPOINT [ "bash", "packaging/deb/entrypoint.sh", "/result" ]