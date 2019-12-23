# http://www.webalice.it/o.drofa/davide/makefile-fortran/makefile-fortran.html

# The compiler
FC = gfortran
# flags for debugging or for maximum performance, comment as necessary
FCFLAGS = -g -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5
FCFLAGS = -O2
# flags forall (e.g. look for system .mod files, required in gfortran)
FCFLAGS += -I/usr/include

# libraries needed for linking, unused in the examples
#LDFLAGS = -li_need_this_lib

# List of executables to be built within the package
PROGRAMS = prog1 prog2 prog3 prog4

# "make" builds all
all: $(PROGRAMS)

prog2.o: prog2.incf

prog3: aux.o

prog4.o: mod.o
prog4: mod.o

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

%.o: %.F90
	$(FC) $(FCFLAGS) -c $<

# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ $(PROGRAMS)
