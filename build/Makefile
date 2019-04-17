prefix = /usr/local

FC = gfortran
ARCH_FLAGS ?= -march=native
FFLAGS = -g -O3 -funsafe-math-optimizations $(ARCH_FLAGS) -fopenmp
FFLAGS_EXTRA = -fno-backtrace -fimplicit-none -ffree-line-length-none \
				  -pedantic -Wall -Wno-unused-dummy-argument -Warray-temporaries

LDLIBS += -lcfitsio
INCLUDE += -I/usr/include

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
	$(FC) $(INCLUDE) $(FFLAGS_EXTRA) $(FFLAGS) -c $< -o $@

$(PROGRAMS): %: $(OBJECTS) %.f90
	$(FC) $(FFLAGS_EXTRA) -fwhole-program $(FFLAGS) $(LDFLAGS) $^ $(LDLIBS) -o $@
$(TESTS): %: $(OBJECTS) %.f90
	$(FC) $(FFLAGS_EXTRA) $(FFLAGS) $(LDFLAGS) $^ $(LDLIBS) -o $@

clean:
	$(RM) *.o *.mod $(PROGRAMS) $(TESTS)
