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
FCFLAGS = -g -c -Wall -Wunused-parameter -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5
PROGRAM = tridiagonal_demo
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
	@echo "\033[1mSRCS\033[0m = $(SRCS)"
	@echo "\033[1mOBJS\033[0m = $(OBJS)"
	@echo "\033[1mMODS\033[0m = $(MODS)"
	@echo "\033[1mMOD_OBJS\033[0m = $(MOD_OBJS)"
	@echo "\033[1mPROGRAM\033[0m  = $(PROGRAM)"
	@echo "\033[1mPRG_OBJ\033[0m  = $(PRG_OBJ)"

clean:
	rm -rf $(OBJS) $(PROGRAM) $(patsubst %.o,%.mod,$(MOD_OBJS))
	rm -f *.mod

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# resolve module interdependencies
mod_constants.o           : mod_precision_definitions.o
mod_tridiagonal_solvers.o : mod_precision_definitions.o mod_constants.o
