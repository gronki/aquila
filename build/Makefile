# Makefile for aquila
# (c) 2021 Dominik Gronkiewicz <gronki@gmail.com>
# on MIT license

#-----------------------------------------------------------------------------#
FC := gfortran
# optimization flags
FFLAGS := -g1 -O3 -funsafe-math-optimizations -march=native -fopenmp
# Fortran language flags (always included)
FFLAGS_LANG := -fimplicit-none -ffree-line-length-none -std=f2018 -pedantic
# debugging flags
FFLAGS_DEBUG := -O0 -g -fcheck=all -D_DEBUG \
	-Wall -Wno-unused-dummy-argument -Warray-temporaries

#-----------------------------------------------------------------------------#

INCLUDE += -I/usr/include

ifeq ($(shell pkg-config --modversion cfitsio),)
	LDLIBS  += -lcfitsio
else
	LDLIBS  := $(LDLIBS)  $(shell pkg-config --libs   cfitsio)
	INCLUDE := $(INCLUDE) $(shell pkg-config --cflags cfitsio)
endif

ifeq ($(shell pkg-config --modversion libpng),)
	LDLIBS  += -lpng
else
	LDLIBS  := $(LDLIBS)  $(shell pkg-config --libs   libpng)
	INCLUDE := $(INCLUDE) $(shell pkg-config --cflags libpng)
endif

ifeq ($(shell pkg-config --modversion fftw3),)
	LDLIBS  += -lfftw3
else
	LDLIBS  := $(LDLIBS)  $(shell pkg-config --libs   fftw3)
	INCLUDE := $(INCLUDE) $(shell pkg-config --cflags fftw3)
endif

#-----------------------------------------------------------------------------#

prefix := $(shell if test -w "/usr/local"; then echo "/usr/local"; \
	else echo "$(HOME)/.local"; fi)

#-----------------------------------------------------------------------------#

VPATH := ../src/modules:../src/modules/framehandling:../src/programs:../src/test
PROGRAMS := aqstack aqlrgb aqlrgb2
TESTS := $(patsubst ../src/test/test_%, test_%, $(basename $(wildcard ../src/test/test_*.[fF]90)))

#-----------------------------------------------------------------------------#

all: $(PROGRAMS)
debug: $(PROGRAMS)
debug $(TESTS): FFLAGS := $(FFLAGS_DEBUG)

install: all
	@echo "installing in $(DESTDIR)$(prefix)"
	install -d $(DESTDIR)$(prefix)/bin
	install $(PROGRAMS) $(DESTDIR)$(prefix)/bin

# to generate new dependencies run: fortdep2 -p -o deps.inc
include deps.inc

%.o: %.f90
	$(FC) $(INCLUDE) $(FFLAGS) $(FFLAGS_LANG) -c $< -o $@
%.o: %.F90
	$(FC) $(INCLUDE) $(FFLAGS) $(FFLAGS_LANG) -c $< -o $@

$(PROGRAMS):
	$(FC) $(FFLAGS) $(LDFLAGS) $^ $(LDLIBS) -o $@
$(TESTS):
	$(FC) $(FFLAGS) $(LDFLAGS) $^ $(LDLIBS) -o $@

clean:
	$(RM) *.o *.mod $(PROGRAMS) test_*

.PHONY: all debug install clean
