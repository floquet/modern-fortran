## Find all source files, create a list of corresponding object files
PROGRAM = lsq

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
FCFLAGS = -g -c ${gflags}
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
# modules with no interdependencies: mod_precision_definitions.mod
# resolve module interdependencies
mod_allocators.o              : mod_precision_definitions.o
mod_intermediates.o           : mod_precision_definitions.o mod_parameters.o mod_measurements.o
mod_matrices.o                : mod_precision_definitions.o mod_parameters.o mod_allocators.o mod_intermediates.o mod_intermediates.o
mod_measurements.o            : mod_precision_definitions.o mod_allocators.o
mod_parameters.o              : mod_precision_definitions.o
mod_queries.o                 : mod_precision_definitions.o
mod_results.o                 : mod_precision_definitions.o mod_parameters.o mod_measurements.o mod_solns_linear.o
mod_solns_linear.o            : mod_precision_definitions.o mod_parameters.o mod_intermediates.o mod_matrices.o mod_matrices.o
mod_validate.o                : mod_precision_definitions.o mod_measurements.o mod_results.o mod_solns_linear.o
mod_sub_validate_loaders.o    : mod_validate.o

# guest@rouson-VirtualBox:~/github/fortran/projects/least squares/bevington/exemplar$ make clean
# rm -rf lsq.o mod_allocators.o mod_intermediates.o mod_matrices.o mod_measurements.o mod_parameters.o mod_precision_definitions.o mod_queries.o mod_results.o mod_solns_linear.o mod_sub_validate_loaders.o mod_validate.o lsq mod_allocators.mod mod_intermediates.mod mod_matrices.mod mod_measurements.mod mod_parameters.mod mod_precision_definitions.mod mod_queries.mod mod_results.mod mod_solns_linear.mod mod_sub_validate_loaders.mod mod_validate.mod
# rm -f *.mod *.smod *.o
# guest@rouson-VirtualBox:~/github/fortran/projects/least squares/bevington/exemplar$ make
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_allocators.o mod_allocators.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_parameters.o mod_parameters.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_measurements.o mod_measurements.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_intermediates.o mod_intermediates.f08
# mod_intermediates.f08:97:17:
#
#              if ( me % det == zero ) then
#                  1
#
# Warning: Equality comparison for REAL(8) at (1)
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_matrices.o mod_matrices.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_queries.o mod_queries.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_solns_linear.o mod_solns_linear.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_results.o mod_results.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_validate.o mod_validate.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o mod_sub_validate_loaders.o mod_sub_validate_loaders.f08
# gfortran -g -c -Wall -Wextra -Wconversion -Og -pedantic -g -fcheck=bounds -fmax-errors=5 -o lsq.o lsq.f08
# gfortran -g -o lsq lsq.o mod_allocators.o mod_intermediates.o mod_matrices.o mod_measurements.o mod_parameters.o mod_precision_definitions.o mod_queries.o mod_results.o mod_solns_linear.o mod_sub_validate_loaders.o mod_validate.o
# guest@rouson-VirtualBox:~/github/fortran/projects/least squares/bevington/exemplar$ make debug
# PROGRAM  = lsq
# PRG_OBJ  = lsq.o
# SRCS     = lsq.f08 mod_allocators.f08 mod_intermediates.f08 mod_matrices.f08 mod_measurements.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_queries.f08 mod_results.f08 mod_solns_linear.f08 mod_sub_validate_loaders.f08 mod_validate.f08
# OBJS     = lsq.o mod_allocators.o mod_intermediates.o mod_matrices.o mod_measurements.o mod_parameters.o mod_precision_definitions.o mod_queries.o mod_results.o mod_solns_linear.o mod_sub_validate_loaders.o mod_validate.o
# MODS     = mod_allocators.f08 mod_intermediates.f08 mod_matrices.f08 mod_measurements.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_queries.f08 mod_results.f08 mod_solns_linear.f08 mod_sub_validate_loaders.f08 mod_validate.f08
# MOD_OBJS = mod_allocators.o mod_intermediates.o mod_matrices.o mod_measurements.o mod_parameters.o mod_precision_definitions.o mod_queries.o mod_results.o mod_solns_linear.o mod_sub_validate_loaders.o mod_validate.o
