# Find all source files, create a list of corresponding object files
PROGRAM = flops

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
mod_file_handling.o        :
mod_harvest_blade.o        :
mod_mpif_sgi.o             :
mod_test_suite.o           : mod_timer_CPU.o mod_timer_clock.o
mod_timer_clock.o          :
mod_timer_CPU.o            :


# rditldmt@ITLDMT-MD-O2034:alpha $ date
# Fri Sep 30 16:59:13 CDT 2016
# rditldmt@ITLDMT-MD-O2034:alpha $ pwd
# /Users/rditldmt/hpc/fortran/tools/moncrief/alpha
# rditldmt@ITLDMT-MD-O2034:alpha $ make debug
# PROGRAM  = flops
# PRG_OBJ  = flops.o
# SRCS     = flops.f08 mod_file_handling.f08 mod_harvest_blade.f08 mod_mpif_sgi.f08 mod_test_suite.f08 mod_timer_CPU.f08 mod_timer_clock.f08
# OBJS     = flops.o mod_file_handling.o mod_harvest_blade.o mod_mpif_sgi.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o
# MODS     = mod_file_handling.f08 mod_harvest_blade.f08 mod_mpif_sgi.f08 mod_test_suite.f08 mod_timer_CPU.f08 mod_timer_clock.f08
# MOD_OBJS = mod_file_handling.o mod_harvest_blade.o mod_mpif_sgi.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o
# rditldmt@ITLDMT-MD-O2034:alpha $
# rditldmt@ITLDMT-MD-O2034:alpha $ make
# mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
# mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_harvest_blade.o mod_harvest_blade.f08
# mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_mpif_sgi.o mod_mpif_sgi.f08
# mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_timer_CPU.o mod_timer_CPU.f08
# mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_timer_clock.o mod_timer_clock.f08
# mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_test_suite.o mod_test_suite.f08
# mpif90 -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o flops.o flops.f08
# mpif90 -g -o flops flops.o mod_file_handling.o mod_harvest_blade.o mod_mpif_sgi.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o

# rditldmt@ITLDMT-MD-O2034:alpha $ gfortran --version
# GNU Fortran (MacPorts gcc7 7-20160925_0) 7.0.0 20160925 (experimental)
# Copyright (C) 2016 Free Software Foundation, Inc.
# This is free software; see the source for copying conditions.  There is NO
# warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
