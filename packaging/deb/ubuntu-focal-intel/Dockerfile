FROM ubuntu:20.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
   gpg gpg-agent wget && apt-get clean

# download the key to system keyring
RUN wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
| gpg --dearmor > /usr/share/keyrings/oneapi-archive-keyring.gpg

RUN echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" \
> /etc/apt/sources.list.d/oneAPI.list

ARG ONEAPI_VER=2025.2

RUN apt-get update && apt-get install -y --no-install-recommends \
git libcfitsio-dev libpng-dev libgnutls30 libreadline-dev \
intel-oneapi-compiler-fortran-${ONEAPI_VER} \
intel-oneapi-compiler-dpcpp-cpp-${ONEAPI_VER} \
&& apt-get clean


ENV FPM_FC=ifx
ENV FPM_CC=icx
ENV FPM_CXX=icpx

WORKDIR /fpm
ADD packaging/fpm.F90 .
RUN . /opt/intel/oneapi/setvars.sh && ${FPM_FC} -O0 fpm.F90 -o fpm \
   && install fpm /usr/bin && rm -f *.mod *.smod *.o fpm
RUN echo "source /opt/intel/oneapi/setvars.sh" > /etc/buildenv.sh

WORKDIR /source

ENV VERSION="unknown"
ENV DEP_PACKAGES="libcfitsio8, libpng16-16, libreadline8"
ENV FPM_FFLAGS="-O3 -fp-model=fast -g -heap-arrays -qopenmp"
ENV FPM_LDFLAGS="-static-intel -qopenmp-link=static"

RUN mkdir -p /result

ENTRYPOINT [ "bash", "packaging/deb/entrypoint.sh", "/result" ]