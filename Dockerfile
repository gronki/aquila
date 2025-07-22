FROM ubuntu:24.04 AS builder

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
ADD https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 fpm.F90
RUN . /opt/intel/oneapi/setvars.sh && ${FPM_FC} -O0 fpm.F90 -o fpm && install fpm /usr/local/bin/
RUN echo "source /opt/intel/oneapi/setvars.sh" > /etc/buildenv.sh

WORKDIR /source
COPY . .

ENV FPM_FFLAGS="-O2 -fp-model=fast -g1"
ENV FPM_LDFLAGS="-static-intel -qopenmp-link=static"

RUN . /opt/intel/oneapi/setvars.sh && \
    rm -rf build && \
    find -name \*.mod -delete && \
    find -name \*.smod -delete && \
    fpm build && fpm install --no-rebuild --prefix=/opt/aquila

FROM ubuntu:24.04 AS runner

ENV DEBIAN_FRONTEND=noninteractive

COPY --from=builder /opt/aquila/ /opt/aquila/

RUN apt-get update && apt-get install -y libcfitsio10 libpng16-16 libreadline8t64 && apt-get clean

ENV PATH="${PATH}:/opt/aquila/bin"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/opt/aquila/lib"

COPY scripts/docker_entrypoint.sh /src/entrypoint.sh
RUN chmod +x /src/entrypoint.sh

WORKDIR /work

ENTRYPOINT [ "/src/entrypoint.sh" ]