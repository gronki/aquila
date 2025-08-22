FROM ubuntu:24.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
git libcfitsio-dev libpng-dev libgnutls30 libreadline-dev \
gfortran-14 gcc-14 \
&& apt-get clean

ENV FPM_FC=gfortran-14
ENV FPM_CC=gcc-14
ENV FPM_CXX=g++-14

WORKDIR /fpm
ADD packaging/fpm.F90 .
RUN ${FPM_FC} -O0 fpm.F90 -o fpm && install fpm /usr/bin && rm -f *.mod *.smod *.o fpm
RUN echo > /etc/buildenv.sh

WORKDIR /source

ENV VERSION="unknown"
ENV DEP_PACKAGES="libcfitsio10, libpng16-16, libreadline8t64, libgomp1, libgfortran5"
ENV FPM_FFLAGS="-O2 -funsafe-math-optimizations -g -fopenmp"
ENV FPM_LDFLAGS=""

RUN mkdir -p /result

ENTRYPOINT [ "bash", "packaging/deb/entrypoint.sh", "/result" ]