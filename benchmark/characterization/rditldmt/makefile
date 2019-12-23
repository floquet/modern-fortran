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

#  19:04 ITL-DTOPA-MP rditldmt $ make debug
# PROGRAM  = characterize
# PRG_OBJ  = characterize.o
# SRCS     = characterize.f08 mod_accumulator.f08 mod_allocations.f08 mod_blocks.f08 mod_character_encodings.f08 mod_constants.f08 mod_declaration_precision.f08 mod_epsilon.f08 mod_iso_variables.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_precision_request.f08 mod_queries.f08 mod_shared.f08 mod_timer_CPU.f08 mod_timer_clock.f08 mod_times.f08 mod_unit_values.f08 mod_variable_types.f08
# OBJS     = characterize.o mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_CPU.o mod_timer_clock.o mod_times.o mod_unit_values.o mod_variable_types.o
# MODS     = mod_accumulator.f08 mod_allocations.f08 mod_blocks.f08 mod_character_encodings.f08 mod_constants.f08 mod_declaration_precision.f08 mod_epsilon.f08 mod_iso_variables.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_precision_request.f08 mod_queries.f08 mod_shared.f08 mod_timer_CPU.f08 mod_timer_clock.f08 mod_times.f08 mod_unit_values.f08 mod_variable_types.f08
# MOD_OBJS = mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_CPU.o mod_timer_clock.o mod_times.o mod_unit_values.o mod_variable_types.o
#  19:04 ITL-DTOPA-MP rditldmt $
#  19:04 ITL-DTOPA-MP rditldmt $ make
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_parameters.o mod_parameters.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_shared.o mod_shared.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_blocks.o mod_blocks.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_timer_clock.o mod_timer_clock.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_timer_CPU.o mod_timer_CPU.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_unit_values.o mod_unit_values.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_accumulator.o mod_accumulator.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_allocations.o mod_allocations.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_character_encodings.o mod_character_encodings.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_declaration_precision.o mod_declaration_precision.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_epsilon.o mod_epsilon.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_iso_variables.o mod_iso_variables.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_request.o mod_precision_request.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_queries.o mod_queries.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_times.o mod_times.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_variable_types.o mod_variable_types.f08
# gfortran -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o characterize.o characterize.f08
# gfortran -g -o characterize characterize.o mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_CPU.o mod_timer_clock.o mod_times.o mod_unit_values.o mod_variable_types.o
#  19:04 ITL-DTOPA-MP rditldmt $
#  19:04 ITL-DTOPA-MP rditldmt $ date
# Fri Apr  8 19:04:32 CDT 2016
#  19:04 ITL-DTOPA-MP rditldmt $ pwd
# /Users/rditldmt/Box Sync/fortran/benchmark/characterization/rditldmt

#  19:05 ITL-DTOPA-MP rditldmt $ gfortran -v
# Using built-in specs.
# COLLECT_GCC=gfortran
# COLLECT_LTO_WRAPPER=/usr/local/gfortran/libexec/gcc/x86_64-apple-darwin14/5.2.0/lto-wrapper
# Target: x86_64-apple-darwin14
# Configured with: ../gcc-5.2.0/configure --prefix=/usr/local/gfortran --with-gmp=/Users/fx/devel/gcc/deps-static/x86_64 --enable-languages=c,c++,fortran,objc,obj-c++ --build=x86_64-apple-darwin14
# Thread model: posix
# gcc version 5.2.0 (GCC) 
