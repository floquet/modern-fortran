# https://stackoverflow.com/questions/35234003/how-to-create-a-makefile-for-a-fortran-program-using-modules
# https://stackoverflow.com/questions/5871780/creating-a-fortran-makefile
# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.f08)
OBJS=$(patsubst %.f08,%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.f08)
MOD_OBJS=$(patsubst %.f08,%.o,$(MODS))

# Compiler/Linker settings
FC = /usr/local/bin/gfortran
FLFLAGS = -g
FCFLAGS = -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5
PROGRAM = tridiagonal
PRG_OBJ = $(PROGRAM).o

# Clean the suffixes
.SUFFIXES:

# Set the suffixes we are interested in
.SUFFIXES: .f08 .o

# make without parameters will make first target found.
default : $(PROGRAM)

# Compiler steps for all objects
%.o : %.f08
	$(FC) $(FCFLAGS) -o $@ $<

# Linker
$(PROGRAM) : $(OBJS)
	$(FC) $(FLFLAGS) -o $@ $^

debug:
	@echo "SRCS = $(SRCS)"
	@echo "OBJS = $(OBJS)"
	@echo "MODS = $(MODS)"
	@echo "MOD_OBJS = $(MOD_OBJS)"
	@echo "PROGRAM = $(PROGRAM)"
	@echo "PRG_OBJ = $(PRG_OBJ)"

clean:
	rm -rf $(OBJS) $(PROGRAM) $(patsubst %.o,%.mod,$(MOD_OBJS))
	rm -f *.mod

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# resolve module interdependencies
mod_constants.o           : mod_precision_definitions.o
mod_parameters.o          : mod_precision_definitions.o mod_constants.o
mod_tridiagonal_solvers.o : mod_precision_definitions.o mod_constants.o mod_parameters.o
