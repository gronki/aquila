FROM fedora:35
RUN dnf install -y gcc-gfortran openblas-devel fftw-devel cfitsio-devel libpng-devel \
    && dnf clean all

WORKDIR /build
COPY . .
RUN cd build && make && make install prefix=/usr

WORKDIR /home
