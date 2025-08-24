. /opt/intel/oneapi/setvars.sh

export FPM_FC=ifx
export FPM_CC=icx
export FPM_CXX=icpx
export DEP_PACKAGES="libcfitsio10, libpng16-16, libreadline8t64"
export FPM_FFLAGS="-O3 -fp-model=fast -g -heap-arrays"
export FPM_LDFLAGS="-static-intel -qopenmp-link=static"