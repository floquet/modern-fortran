# Find all source files, create a list of corresponding object files
PROGRAM = apex

EXT = f08
SRCS=$(wildcard *.$(EXT))
OBJS=$(patsubst %.$(EXT),%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.$(EXT))
MOD_OBJS=$(patsubst %.$(EXT),%.o,$(MODS))

# Compiler/Linker settings
# first run alias gogcc
FC = ${makefc}
FLFLAGS = -g
FCFLAGS = -c ${makefc_flags} 
PRG_OBJ = $(PROGRAM).o

# Clean the suffixes
.SUFFIXES:

# Set the suffixes we are interested in
.SUFFIXES: .$(EXT) .o
#all : apex
# make without parameters will make first target found.
default : $(PROGRAM)
all     : $(PROGRAM)  # eclipse

# Compiler steps for all objects
%.o : %.$(EXT)
	$(FC) $(FCFLAGS) -o $@ $<

# Linker
$(PROGRAM) : $(OBJS)
	$(FC) $(FLFLAGS) -o $@ $^

debug:
	@echo "PROGRAM  = $(PROGRAM)"
	@echo "PRG_OBJ  = $(PRG_OBJ)"
	@echo "SRCS     = $(SRCS)"
	@echo "OBJS     = $(OBJS)"
	@echo "MODS     = $(MODS)"
	@echo "MOD_OBJS = $(MOD_OBJS)"

clean:
	rm -rf $(OBJS) $(PROGRAM) $(patsubst %.o,%.mod,$(MOD_OBJS))
	rm -f *.mod *.smod *.o

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# resolve module interdependencies
# modules with no dependencies: mod_precision_definitions, mod_filehandling
mod_allocators.o            : mod_precision_definitions.o
mod_angle.o                 : mod_precision_definitions.o mod_constants.o
mod_axes.o                  : mod_precision_definitions.o mod_constants.o mod_allocators.o mod_filehandling.o mod_LSQ.o mod_matrix_writer.o mod_parameters_simulation.o mod_read_LAMMPS.o
mod_constants.o             : mod_precision_definitions.o
mod_grain.o                 : mod_precision_definitions.o mod_constants.o mod_angle.o mod_axes.o
mod_LSQ.o                   : mod_precision_definitions.o mod_constants.o
mod_mathematica_output.o    : mod_precision_definitions.o                 mod_angle.o mod_LSQ.o
mod_matrix_writer.o         : mod_precision_definitions.o                 mod_filehandling.o
mod_parameters_simulation.o : mod_precision_definitions.o
mode_read_LAMMPS.o          : mod_precision_definitions.o mod_constants.o mod_filehandling.o mod_parameters_simulation.o
mod_validate_apex.o         : mod_precision_definitions.o mod_constants.o mod_angle.o mod_mathematica_output.o mod_parameters_simulation.o
mod_validate_lines.o        : mod_precision_definitions.o mod_constants.o mod_LSQ.o   mod_mathematica_output.o mod_parameters_simulation.o

# guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ date
# Fri Apr  1 17:02:19 PDT 2016
# guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ pwd
# /home/guest/github/fortran/projects/crystals/apex_angles/delta
# guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ make clean
# rm -rf apex.o mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_aandp.o mod_axes_sub_IO.o mod_constants.o mod_filehandling.o mod_grain.o mod_LSQ.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o apex mod_allocators.mod mod_angle.mod mod_axes.mod mod_axes_sub_aandp.mod mod_axes_sub_IO.mod mod_constants.mod mod_filehandling.mod mod_grain.mod mod_LSQ.mod mod_mathematica_output.mod mod_matrix_writer.mod mod_parameters_simulation.mod mod_precision_definitions.mod mod_read_LAMMPS.mod mod_validate_apex.mod mod_validate_lines.mod
# rm -f *.mod *.smod *.o
# guest@rouson-VirtualBox:~/github/fortran/projects/crystals/apex_angles/delta$ make debug
# PROGRAM  = apex
# PRG_OBJ  = apex.o
# SRCS     = apex.f08 mod_allocators.f08 mod_angle.f08 mod_axes.f08 mod_axes_sub_aandp.f08 mod_axes_sub_IO.f08 mod_constants.f08 mod_filehandling.f08 mod_grain.f08 mod_LSQ.f08 mod_mathematica_output.f08 mod_matrix_writer.f08 mod_parameters_simulation.f08 mod_precision_definitions.f08 mod_read_LAMMPS.f08 mod_validate_apex.f08 mod_validate_lines.f08
# OBJS     = apex.o mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_aandp.o mod_axes_sub_IO.o mod_constants.o mod_filehandling.o mod_grain.o mod_LSQ.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o
# MODS     = mod_allocators.f08 mod_angle.f08 mod_axes.f08 mod_axes_sub_aandp.f08 mod_axes_sub_IO.f08 mod_constants.f08 mod_filehandling.f08 mod_grain.f08 mod_LSQ.f08 mod_mathematica_output.f08 mod_matrix_writer.f08 mod_parameters_simulation.f08 mod_precision_definitions.f08 mod_read_LAMMPS.f08 mod_validate_apex.f08 mod_validate_lines.f08
# MOD_OBJS = mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_aandp.o mod_axes_sub_IO.o mod_constants.o mod_filehandling.o mod_grain.o mod_LSQ.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o
