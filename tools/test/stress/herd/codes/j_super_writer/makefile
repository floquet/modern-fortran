# Find all source files, create a list of corresponding object files
PROGRAM = machine_pbs

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
mod_components.o            : mod_file_handling.o
# mod_angle.o                 : mod_precision_definitions.o mod_constants.o
# mod_axes.o                  : mod_precision_definitions.o mod_constants.o mod_allocators.o mod_filehandling.o mod_LSQ.o mod_matrix_writer.o mod_parameters_simulation.o mod_read_LAMMPS.o
# mod_constants.o             : mod_precision_definitions.o
# mod_grain.o                 : mod_precision_definitions.o mod_constants.o mod_angle.o mod_axes.o
# mod_LSQ.o                   : mod_precision_definitions.o mod_constants.o
# mod_mathematica_output.o    : mod_precision_definitions.o                 mod_angle.o mod_LSQ.o
# mod_matrix_writer.o         : mod_precision_definitions.o                 mod_filehandling.o
# mod_parameters_simulation.o : mod_precision_definitions.o
# mode_read_LAMMPS.o          : mod_precision_definitions.o mod_constants.o mod_filehandling.o mod_parameters_simulation.o
# mod_validate_apex.o         : mod_precision_definitions.o mod_constants.o mod_angle.o mod_mathematica_output.o mod_parameters_simulation.o
# mod_validate_lines.o        : mod_precision_definitions.o mod_constants.o mod_LSQ.o   mod_mathematica_output.o mod_parameters_simulation.o
