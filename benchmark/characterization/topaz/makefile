# module switch compiler/intel/15.0.3 compiler/gcc/5.3.0

# Find all source files, create a list of corresponding object files
PROGRAM = characterize

EXT = f08
# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.$(EXT))
OBJS=$(patsubst %.$(EXT),%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.$(EXT))
MOD_OBJS=$(patsubst %.$(EXT),%.o,$(MODS))

# Compiler/Linker settings
# gfortran - gnu (execution: ./a.out) (flags: gflags in ~/.bashrc)
# mpif90 - gnu, MPI Mac Desktop (execution: mpirun -np 4 ./a.out)
# ftn - Cray, (aprun -n 64 ./a.out)
FC = gfortran
FLFLAGS = -g
FCFLAGS = -c ${gflags}
PRG_OBJ = $(PROGRAM).o

# Clean the suffixes
.SUFFIXES:

# Set the suffixes we are interested in
.SUFFIXES: .$(EXT) .o

# make without parameters will make first target found.
default : $(PROGRAM)

# Compiler steps for all objects
$(OBJS) : %.o : %.$(EXT)
	$(FC) $(FCFLAGS) -o $@ $<

# Linker
$(PROGRAM) : $(OBJS)
	$(FC) $(FLFLAGS) -o $@ $^

debug:
	@echo "${bold}PROGRAM${normal}  = $(PROGRAM)"
	@echo "${bold}PRG_OBJ${normal}  = $(PRG_OBJ)"
	@echo "${bold}SRCS${normal}     = $(SRCS)"
	@echo "${bold}OBJS${normal}     = $(OBJS)"
	@echo "${bold}MODS${normal}     = $(MODS)"
	@echo "${bold}MOD_OBJS${normal} = $(MOD_OBJS)"

clean:
	rm -rf $(OBJS) $(PROGRAM) $(patsubst %.o,%.mod,$(MOD_OBJS))
	rm -f *.mod *.smod *.o

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# Blocks and allocations depends on shared
# mod_unit_values, mod_timer_CPU have no dependencies
# strict capitalization needed for portability
mod_accumulator.o           : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_timer_clock.o mod_timer_CPU.o mod_unit_values.o
mod_allocations.o           : mod_precision_definitions.o mod_parameters.o mod_timer_clock.o mod_timer_CPU.o mod_shared.o mod_blocks.o mod_unit_values.o
mod_blocks.o                : mod_precision_definitions.o mod_parameters.o mod_shared.o
mod_character_encodings.o   : mod_precision_definitions.o mod_blocks.o mod_shared.o
mod_constants.o             : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_unit_values.o
mod_declaration_precision.o : mod_precision_definitions.o
mod_epsilon.o               : mod_precision_definitions.o mod_shared.o mod_blocks.o mod_unit_values.o
mod_iso_variables.o         : mod_precision_definitions.o mod_blocks.o mod_shared.o
mod_parameters.o            : mod_precision_definitions.o
mod_precision_request.o     : mod_precision_definitions.o
mod_queries.o               : mod_precision_definitions.o mod_parameters.o mod_blocks.o mod_shared.o mod_unit_values.o
mod_shared.o                : mod_precision_definitions.o mod_parameters.o
mod_timer_clock.o           : mod_parameters.o
mod_times.o                 : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_timer_CPU.o mod_timer_clock.o
mod_variable_types.o        : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_unit_values.o
