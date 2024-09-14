FROM debian:bookworm AS builder
RUN apt-get update && apt-get install -y --no-install-recommends gfortran-12 libcfitsio-dev libpng-dev && apt-get clean

WORKDIR /fpm
ADD https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 fpm.F90
RUN gfortran-12 -O0 fpm.F90 -o fpm && install fpm /usr/local/bin/

WORKDIR /source
COPY . .
ENV FPM_FC=gfortran-12
ARG FFLAGS="-O3 -funsafe-math-optimizations -g1 -fopenmp"
ENV FPM_FFLAGS="${FFLAGS}"
RUN fpm build --verbose && fpm install --prefix /build

FROM debian:bookworm

RUN apt-get update && \
    apt-get install -y --no-install-recommends libcfitsio10 libpng16-16 libgfortran5 libgomp1 && \
    apt-get clean
COPY --from=builder /build/ /usr/

WORKDIR /work
