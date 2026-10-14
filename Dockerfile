FROM ubuntu:24.04 AS builder
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake make gcc g++ gfortran libreadline-dev pkg-config zlib1g-dev

WORKDIR /tmp/aquila
COPY . .
WORKDIR /tmp/aquila/build
RUN cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/opt/aquila/usr \
        -DAQUILA_PARALLEL=On -DAQUILA_TESTS=Off -DAQUILA_WINDOW=Off \
    && cmake --build . -j"$(nproc)" && cmake --install .

COPY scripts/entrypoint.sh /opt/aquila/entrypoint.sh
RUN chmod +x /opt/aquila/entrypoint.sh

FROM ubuntu:24.04
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    libreadline8t64 libgomp1 libgfortran5 zlib1g

COPY --from=builder /opt/aquila/ /opt/aquila/
WORKDIR /work

# Recommended way to run (e.g. as an alias in .bashrc):
#   alias aquila='docker run -it --rm --user "$(id -u):$(id -g)" -v aquila-history:/history -v "$PWD":/work aquila'
#   aquila cli
# --user: output files in the current directory are owned by you, not root
# aquila-history: a named volume keeping aqcli history between runs
# /history is world-writable so that history works under any --user
RUN mkdir -m 1777 /history
ENV AQUILA_HISTORY_FILE=/history/history.txt

ENTRYPOINT [ "/opt/aquila/entrypoint.sh" ]
