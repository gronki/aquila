FROM fedora:latest AS builder
RUN dnf install -y gcc-gfortran fedora-packager rpmdevtools gdb cfitsio-devel libpng-devel


# ARG MY_UID=1000
# ARG MY_GID=1000
# ARG USERNAME=user
# RUN groupadd -g ${MY_GID} "${USERNAME}" && useradd -u ${MY_UID} -g ${MY_GID} "${USERNAME}"
# USER "${USERNAME}"

RUN echo "%_topdir /rpmbuild" >> "${HOME}/.rpmmacros"
RUN rpmdev-setuptree && find /rpmbuild -type d -print | xargs chmod 777

ENV VERSION="unknown"
ENV FPM_FC=gfortran

WORKDIR /source
ENTRYPOINT [ "bash", "packaging/rpm/entrypoint.sh" ]