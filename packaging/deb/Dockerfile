ARG ARG_DISTRO=debian
ARG ARG_DISTRO_VERSION=sid

FROM ${ARG_DISTRO}:${ARG_DISTRO_VERSION} AS builder
ARG ARG_DISTRO
ARG ARG_DISTRO_VERSION

ENV DEBIAN_FRONTEND=noninteractive
ARG ARG_GCC_VERSION=14
ENV GCC_VERSION="${ARG_GCC_VERSION}"

RUN apt-get update && apt-get install -y --no-install-recommends \
    devscripts build-essential debhelper git "gfortran-${ARG_GCC_VERSION}" \
    libcfitsio-dev libpng-dev && \
    apt-get clean

# ARG MY_UID=1000
# ARG MY_GID=1000
# ARG USERNAME=user
# RUN groupadd -g ${MY_GID} "${USERNAME}" && useradd -u ${MY_UID} -g ${MY_GID} "${USERNAME}"
# RUN mkdir -p /result && chown ${MY_UID}:${MY_GID} /result
WORKDIR /source
VOLUME ["/source"]
# USER "${USERNAME}"

ENV DISTRO=${ARG_DISTRO}
ENV DISTRO_VERSION=${ARG_DISTRO_VERSION}
ARG ARG_REQUIRES="libcfitsio10, libpng16-16, libgomp1, libgfortran5"
ENV REQUIRES=${ARG_REQUIRES}
ENV FPM_FC="gfortran-${ARG_GCC_VERSION}"
ENV VERSION="unknown"

VOLUME ["/result"]
ENTRYPOINT [ "bash", "packaging/deb/entrypoint.sh", "/result" ]