# Makefile for aquila
# (c) 2019 Dominik Gronkiewicz <gronki@gmail.com>
# on MIT license

FC := gfortran
ARCHFLAGS ?= -march=native
FFLAGS = -O3 -funsafe-math-optimizations $(ARCHFLAGS) -fopenmp
FFLAGS_EXTRA := -fno-backtrace -fimplicit-none -ffree-line-length-none \
				  -pedantic -Wall -Wno-unused-dummy-argument -Warray-temporaries

LDLIBS += -lcfitsio
INCLUDE += -I/usr/include

ifeq ($(USER),root)
prefix = /usr/local
else
prefix = $(HOME)/.local
endif

VPATH = ../src/modules:../src/programs:../src/test
PROGRAMS = aqstack aqlrgb
TESTS = test_transforms test_background test_kernels test_findstar test_legacy_align \
			test_framehandling test_addsuffix test_deconvolution
OBJECTS = kernels.o statistics.o convolution.o globals.o transforms.o \
				  findstar.o legacy_align.o framehandling.o

all: $(PROGRAMS)

debug: $(TESTS) all
debug: FFLAGS = -g -Og -fcheck=all -fbacktrace

install: all
	install -d $(prefix)/bin
	install $(PROGRAMS) $(prefix)/bin

install-home: override prefix = $(HOME)/.local
install-home: install

include deps.inc

%.o: %.f90
	$(FC) $(INCLUDE) $(FFLAGS) $(FFLAGS_EXTRA) -c $< -o $@

$(PROGRAMS): %: $(OBJECTS) %.f90
	$(FC) $(FFLAGS) $(FFLAGS_EXTRA) $(LDFLAGS) $^ $(LDLIBS) -o $@
$(TESTS): %: $(OBJECTS) %.f90
	$(FC) $(FFLAGS) $(FFLAGS_EXTRA) $(LDFLAGS) $^ $(LDLIBS) -o $@

clean:
	$(RM) *.o *.mod $(PROGRAMS) $(TESTS)
