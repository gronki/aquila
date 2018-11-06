prefix = /usr/local

FC = gfortran
ARCH_FLAGS ?= -march=native
FFLAGS = -g -O3 -funsafe-math-optimizations $(ARCH_FLAGS) -fopenmp
override FFLAGS += -fno-backtrace -fimplicit-none -ffree-line-length-none \
				  -pedantic -Wall -Wno-unused-dummy-argument -Warray-temporaries

LDLIBS += -lcfitsio
INCLUDE += -I/usr/include

VPATH = ../src/modules:../src/programs:../src/test
PROGRAMS = aqstack aqlrgb
TESTS = test_transforms test_background test_kernels test_findstar test_legacy_align \
			test_framehandling test_addsuffix test_deconvolution
OBJECTS = kernels.o statistics.o convolution.o globals.o transforms.o \
				  findstar.o legacy_align.o framehandling.o

all: $(PROGRAMS) $(TESTS)

install: all
	install -d $(prefix)/bin
	install $(PROGRAMS) $(prefix)/bin

install-home: override prefix = $(HOME)/.local
install-home: install

include deps.inc

%.o: %.f90
	$(FC) $(INCLUDE) $(FFLAGS) -c $< -o $@

$(PROGRAMS) $(TESTS): %: $(OBJECTS) %.o
	$(FC) $(FFLAGS) $(LDFLAGS) $^ $(LDLIBS) -o $@

clean:
	$(RM) *.o *.mod $(PROGRAMS) $(TESTS)
