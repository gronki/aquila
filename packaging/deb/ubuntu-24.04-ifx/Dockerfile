FROM intel/oneapi-hpckit:2025.1.0-0-devel-ubuntu24.04 AS builder

ENV FPM_FC=ifx
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends git libcfitsio-dev libpng-dev libgnutls30 libreadline-dev && apt-get clean

WORKDIR /fpm
ADD https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 fpm.F90
RUN ${FPM_FC} -O0 fpm.F90 -o fpm && install fpm /usr/local/bin/

WORKDIR /source

ENV VERSION="unknown"
ENV DEP_PACKAGES="libcfitsio10, libpng16-16, libreadline8t64"
ENV FPM_FFLAGS="-O2 -fp-model=fast -g1 -qopenmp -mavx2"
ENV FPM_LDFLAGS="-static-intel -qopenmp-link=static"

RUN mkdir -p /result

ENTRYPOINT [ "bash", "packaging/deb/entrypoint.sh", "/result" ]