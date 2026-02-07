FROM debian:trixie AS builder
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
cmake make gcc g++ gfortran libpng-dev libreadline-dev pkg-config

WORKDIR /tmp/aquila
COPY . .
WORKDIR /tmp/aquila/build
RUN cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/opt/aquila -DOPENMP=On \
    && cmake --build . && ctest . && cmake --install .

FROM debian:trixie
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpng16-16t64 libreadline8t64 libgomp1 libgfortran5

COPY --from=builder /opt/aquila/ /opt/aquila/
ENV PATH="${PATH}:/opt/aquila/bin"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/opt/aquila/lib"
ENV INCLUDE_PATH="${INCLUDE_PATH}:/opt/aquila/include"

COPY scripts/docker_entrypoint.sh /opt/aquila/docker_entrypoint.sh
RUN chmod +x /opt/aquila/docker_entrypoint.sh

ENTRYPOINT [ "/opt/aquila/docker_entrypoint.sh" ]