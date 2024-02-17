FROM intel/oneapi-hpckit AS builder
RUN apt-get update && apt-get install -y libcfitsio-dev libpng-dev && apt-get clean

WORKDIR /fpm
ADD https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 fpm.F90
RUN ifort -O fpm.F90 -o fpm && install fpm /usr/local/bin/

WORKDIR /build
COPY . .
ENV FPM_FC=ifort
RUN fpm install --profile release --prefix /build/result

FROM intel/oneapi-runtime

RUN apt-get update && apt-get install -y libcfitsio9 libpng16-16 && apt-get clean
COPY --from=builder /build/result/ /usr/

WORKDIR /home