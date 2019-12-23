# Find all source files, create a list of corresponding object files
PROGRAM = cpu

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
FC = ${fortran_compiler_mpi_gnu}
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
mod_test_suite.o           : mod_averager.o mod_timer_CPU.o mod_timer_clock.o

# rditldmt@ITL-DTOPA-MP:topaz $ make debug
# PROGRAM  = cpu
# PRG_OBJ  = cpu.o
# SRCS     = cpu.f08 mod_averager.f08 mod_file_handling.f08 mod_test_suite.f08 mod_timer_CPU.f08 mod_timer_clock.f08 mod_timestamp.f08
# OBJS     = cpu.o mod_averager.o mod_file_handling.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o mod_timestamp.o
# MODS     = mod_averager.f08 mod_file_handling.f08 mod_test_suite.f08 mod_timer_CPU.f08 mod_timer_clock.f08 mod_timestamp.f08
# MOD_OBJS = mod_averager.o mod_file_handling.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o mod_timestamp.o
# rditldmt@ITL-DTOPA-MP:topaz $ make
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_averager.o mod_averager.f08
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_timer_CPU.o mod_timer_CPU.f08
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_timer_clock.o mod_timer_clock.f08
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_test_suite.o mod_test_suite.f08
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_timestamp.o mod_timestamp.f08
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o cpu.o cpu.f08
# mpif90 -g -o cpu cpu.o mod_averager.o mod_file_handling.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o mod_timestamp.o
