FROM intel/hpckit:2024.2.1-0-devel-ubuntu22.04 AS builder
RUN apt-get update && apt-get install -y --no-install-recommends libcfitsio-dev libpng-dev git && apt-get clean

WORKDIR /fpm
ADD https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 fpm.F90
RUN ifort -O0 fpm.F90 -o fpm && install fpm /usr/local/bin/

WORKDIR /source
COPY . .
ENV FPM_FC=ifx
ARG FFLAGS="-O2 -g -fp-model=fast -qopenmp"
ENV FPM_FFLAGS="${FFLAGS}"
RUN fpm install --prefix /build

FROM intel/oneapi-runtime:2024.2.0-1-devel-ubuntu22.04

RUN apt-get update && apt-get install -y --no-install-recommends libcfitsio9 libpng16-16 && apt-get clean
COPY --from=builder /build/ /usr/

WORKDIR /work