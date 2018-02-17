prefix = /usr/local
FC = gfortran
FFLAGS ?= -g -O2
FFLAGS += -Wall -Warray-temporaries -Wrealloc-lhs
LDLIBS = -lcfitsio
VPATH = ../src:../test

OBJECTS = align.o filters.o findstar.o globals.o transforms.o
PROGRAMS = aqfindstar aqalign2 background
TESTS = test_transforms

all: $(PROGRAMS)

tests: $(TESTS)

install: all
	install -d $(prefix)/bin
	install $(PROGRAMS) $(prefix)/bin

install-user: prefix = $(HOME)/.local
install-user: install

align.o         : globals.o transforms.o
aqalign2.o      : align.o filters.o
background.o aqalign2.o: globals.o findstar.o
aqfindstar.o    : findstar.o filters.o
test_transforms.o: transforms.o
example.o transforms.o filters.o findstar.o: globals.o


%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

$(PROGRAMS) $(TESTS): %: $(OBJECTS) %.f90
	$(FC) $(FFLAGS) $^ $(LDLIBS) -o $@

clean:
	$(RM) *.o *.mod $(PROGRAMS) $(TESTS)
