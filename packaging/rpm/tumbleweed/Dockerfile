ARG ARG_IMAGE_NAME="opensuse/tumbleweed"
ARG ARG_IMAGE_TAG="latest"

FROM ${ARG_IMAGE_NAME}:${ARG_IMAGE_TAG} AS builder

ARG ARG_REQUIRES
ENV REQUIRES="${ARG_REQUIRES}"
ARG ARG_BUILD_REQUIRES
ENV BUILD_REQUIRES="${ARG_BUILD_REQUIRES}"

RUN zypper install -y ${ARG_BUILD_REQUIRES} rpm-build gdb 

RUN echo "%_topdir /rpmbuild" >> "${HOME}/.rpmmacros"
RUN mkdir -p /rpmbuild/{BUILD,RPMS,SOURCES,SPECS,SRPMS} &&  \
    find /rpmbuild -type d -print | xargs chmod 777

ENV VERSION="unknown"
ARG ARG_FC="gfortran"
ENV FPM_FC="${ARG_FC}"
ENV FC="${ARG_FC}"


WORKDIR /source
ENTRYPOINT [ "bash", "packaging/rpm/entrypoint.sh" ]