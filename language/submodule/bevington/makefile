# https://stackoverflow.com/questions/35234003/how-to-create-a-makefile-for-a-fortran-program-using-modules
# https://stackoverflow.com/questions/5871780/creating-a-fortran-makefile
# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.f08)
OBJS=$(patsubst %.f08,%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.f08)
MOD_OBJS=$(patsubst %.f08,%.o,$(MODS))

# Compiler/Linker settings
FC = ftn
FLFLAGS = -g
FCFLAGS = -g -c
PROGRAM = lsq
PRG_OBJ = $(PROGRAM).o

# Clean the suffixes
.SUFFIXES:

# Set the suffixes we are interested in
.SUFFIXES: .f08 .o
all : lsq
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
mod_intermediates.o : mod_precision_definitions.o mod_parameters.o mod_measurements.o
mod_matrices.o      : mod_precision_definitions.o mod_parameters.o mod_intermediates.o
mod_measurements.o  : mod_precision_definitions.o mod_parameters.o
mod_parameters.o    : mod_precision_definitions.o
mod_queries.o       : mod_precision_definitions.o
mod_results.o       : mod_precision_definitions.o mod_parameters.o mod_measurements.o mod_solns_linear.o
mod_solns_linear.o  : mod_precision_definitions.o mod_parameters.o mod_intermediates.o
mod_validate.o      : mod_results.o
