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

# rditldmt@ITL-DTOPA-MP:foxtrot $ date
# Fri Aug 19 11:05:38 CDT 2016
# rditldmt@ITL-DTOPA-MP:foxtrot $ pwd
# /Users/rditldmt/hpc/fortran/projects/grains/crystals/apex_angles/foxtrot
# rditldmt@ITL-DTOPA-MP:foxtrot $ make debug
# PROGRAM  = apex
# PRG_OBJ  = apex.o
# SRCS     = apex.f08 mod_LSQ.f08 mod_allocators.f08 mod_angle.f08 mod_axes.f08 mod_axes_sub_IO.f08 mod_axes_sub_aandp.f08 mod_constants.f08 mod_filehandling.f08 mod_grain.f08 mod_mathematica_output.f08 mod_matrix_writer.f08 mod_parameters_simulation.f08 mod_precision_definitions.f08 mod_read_LAMMPS.f08 mod_validate_apex.f08 mod_validate_lines.f08
# OBJS     = apex.o mod_LSQ.o mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_IO.o mod_axes_sub_aandp.o mod_constants.o mod_filehandling.o mod_grain.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o
# MODS     = mod_LSQ.f08 mod_allocators.f08 mod_angle.f08 mod_axes.f08 mod_axes_sub_IO.f08 mod_axes_sub_aandp.f08 mod_constants.f08 mod_filehandling.f08 mod_grain.f08 mod_mathematica_output.f08 mod_matrix_writer.f08 mod_parameters_simulation.f08 mod_precision_definitions.f08 mod_read_LAMMPS.f08 mod_validate_apex.f08 mod_validate_lines.f08
# MOD_OBJS = mod_LSQ.o mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_IO.o mod_axes_sub_aandp.o mod_constants.o mod_filehandling.o mod_grain.o mod_mathematica_output.o mod_matrix_writer.o mod_parameters_simulation.o mod_precision_definitions.o mod_read_LAMMPS.o mod_validate_apex.o mod_validate_lines.o
# rditldmt@ITL-DTOPA-MP:foxtrot $ make
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_precision_definitions.o mod_precision_definitions.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_constants.o mod_constants.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_LSQ.o mod_LSQ.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_allocators.o mod_allocators.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_angle.o mod_angle.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_filehandling.o mod_filehandling.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_matrix_writer.o mod_matrix_writer.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_parameters_simulation.o mod_parameters_simulation.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_read_LAMMPS.o mod_read_LAMMPS.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_axes.o mod_axes.f08
# mod_axes.f08:21:36:
#
#      integer ( ip ), private :: first, last
#                                     1
# Warning: Unused PRIVATE module variable 'first' declared at (1) [-Wunused-value]
# mod_axes.f08:21:42:
#
#      integer ( ip ), private :: first, last
#                                           1
# Warning: Unused PRIVATE module variable 'last' declared at (1) [-Wunused-value]
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_axes_sub_IO.o mod_axes_sub_IO.f08
# mod_axes_sub_IO.f08:2:17:
#
#  submodule ( mAxes ) smAxesIO
#                  1
# Warning: USE statement at (1) has no ONLY qualifier [-Wuse-without-only]
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_axes_sub_aandp.o mod_axes_sub_aandp.f08
# mod_axes_sub_aandp.f08:2:17:
#
#  submodule ( mAxes ) smAllocPop
#                  1
# Warning: USE statement at (1) has no ONLY qualifier [-Wuse-without-only]
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_grain.o mod_grain.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_mathematica_output.o mod_mathematica_output.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_validate_apex.o mod_validate_apex.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o mod_validate_lines.o mod_validate_lines.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only  -o apex.o apex.f08
# gfortran -g -o apex apex.o mod_LSQ.o mod_allocators.o mod_angle.o mod_axes.o mod_axes_sub_IO.o mod_axes_sub_aandp.o mod_constants.o mod_filehandling.o mod_grain.o mod_mathematica_output.o mod_m
