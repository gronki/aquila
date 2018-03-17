prefix = /usr/local
FC = gfortran
FFLAGS := -O3 -march=native
FFLAGS += -g -Wall -Wno-unused -Warray-temporaries -Wrealloc-lhs
LDLIBS += -lcfitsio
VPATH = ../src/modules:../src/programs:../src/test

PROGRAMS = example background test
TESTS = test_transforms
OBJECTS = kernels.o statistics.o convolution.o globals.o transforms.o findstar.o

all: $(PROGRAMS)

tests: $(TESTS)

install: all
	install -d $(prefix)/bin
	install $(PROGRAMS) $(prefix)/bin

install-user: prefix = $(HOME)/.local
install-user: install

include deps.inc

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(PROGRAMS) $(TESTS): %: $(OBJECTS) %.o
	$(FC) $(FFLAGS) $^ $(LDLIBS) -o $@

clean:
	$(RM) *.o *.mod $(PROGRAMS) $(TESTS)
