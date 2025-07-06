FROM ubuntu:24.04 AS builder

ENV FPM_FC=gfortran-14
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends git gfortran-14 libcfitsio-dev libpng-dev libreadline-dev && apt-get clean

WORKDIR /fpm
ADD https://github.com/fortran-lang/fpm/releases/download/v0.11.0/fpm-0.11.0.F90 fpm.F90
RUN ${FPM_FC} -O0 fpm.F90 -o fpm && install fpm /usr/local/bin/

WORKDIR /source

ENV VERSION="unknown"
ENV DEP_PACKAGES="libcfitsio10, libpng16-16, libgfortran5, libgomp1, libreadline8t64"
ENV FPM_FFLAGS="-O3 -funsafe-math-optimizations -g1 -fopenmp -mavx2"
ENV FPM_LDFLAGS=""

RUN mkdir -p /result

ENTRYPOINT [ "bash", "packaging/deb/entrypoint.sh", "/result" ]
