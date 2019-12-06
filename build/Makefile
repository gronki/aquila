# Makefile for aquila
# (c) 2019 Dominik Gronkiewicz <gronki@gmail.com>
# on MIT license

#-----------------------------------------------------------------------------#

FC := gfortran
ARCHFLAGS ?= -march=native
FFLAGS := -O3 -funsafe-math-optimizations $(ARCHFLAGS) -fopenmp
FFLAGS += -fimplicit-none -ffree-line-length-none -std=f2018 \
				  -pedantic -Wall -Wno-unused-dummy-argument -Warray-temporaries

#-----------------------------------------------------------------------------#

LDLIBS :=
INCLUDE :=

ifeq ($(shell pkg-config --modversion cfitsio),)
LDLIBS += -lcfitsio
else
LDLIBS += $(shell pkg-config --libs cfitsio)
INCLUDE += $(shell pkg-config --cflags cfitsio)
endif

ifeq ($(shell pkg-config --modversion libpng),)
LDLIBS += -lpng
else
LDLIBS += $(shell pkg-config --libs libpng)
INCLUDE += $(shell pkg-config --cflags libpng)
endif

# ifeq ($(shell pkg-config --modversion fftw3),)
# LDLIBS += -lfftw3
# else
# LDLIBS += $(shell pkg-config --libs fftw3)
# INCLUDE += $(shell pkg-config --cflags fftw3)
# endif

#-----------------------------------------------------------------------------#

prefix := $(shell if test -w "/usr/local"; then echo "/usr/local"; \
	else echo "$$HOME/.local"; fi)

#-----------------------------------------------------------------------------#

VPATH := ../src/modules:../src/programs:../src/test
PROGRAMS := aqstack aqlrgb
OBJECTS := kernels.o statistics.o convolution.o globals.o new_align.o \
				  findstar.o legacy_align.o framehandling.o png.o hotpixel.o

#-----------------------------------------------------------------------------#

all: $(PROGRAMS)
debug: $(PROGRAMS)
debug test_%: FFLAGS := -g -fcheck=all -D_DEBUG

install: all
	install -d $(prefix)/bin
	install $(PROGRAMS) $(prefix)/bin

.PHONY: all debug install clean

include deps.inc

%.o: %.f90
	$(FC) $(INCLUDE) $(FFLAGS) -c $< -o $@
%.o: %.F90
	$(FC) $(INCLUDE) $(CPPFLAGS) $(FFLAGS) -c $< -o $@

$(PROGRAMS): LDFLAGS += -fwhole-program
$(PROGRAMS): %: $(OBJECTS) %.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ $(LDLIBS) -o $@
test_%: $(OBJECTS) test_%.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ $(LDLIBS) -o $@

clean:
	$(RM) *.o *.mod $(PROGRAMS) test_*
