# Find all source files, create a list of corresponding object files
PROGRAM = lapack_demo_svd

EXT = f08
# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.$(EXT))
OBJS=$(patsubst %.$(EXT),%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.$(EXT))
MOD_OBJS=$(patsubst %.$(EXT),%.o,$(MODS))

# Compiler/Linker settings
# gfortran - gnu (execution: ./a.out) (flags: gflags in ~/.bashrc)
FC = gfortran
FLFLAGS = -g -framework Accelerate
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
mod_file_handling.o           :
mod_set_precision.o           :
mod_matrix_writer.o           : mod_set_precision.o mod_file_handling.o
mod_matrix_reader.o           : mod_set_precision.o mod_svd_parameters.o
mod_svd_parameters.o          : mod_set_precision.o

# rditldmt@ITL-DTOPA-MP:bravo $ date
# Mon Sep 19 15:28:19 CDT 2016
# rditldmt@ITL-DTOPA-MP:bravo $ pwd
# /Users/rditldmt/hpc/fortran/nla/SVD/lapack/bravo
# rditldmt@ITL-DTOPA-MP:bravo $
# rditldmt@ITL-DTOPA-MP:bravo $ make debug
# PROGRAM  = lapack_demo_svd
# PRG_OBJ  = lapack_demo_svd.o
# SRCS     = lapack_demo_svd.f08 mod_file_handling.f08 mod_matrix_reader.f08 mod_matrix_writer.f08 mod_set_precision.f08 mod_svd_parameters.f08
# OBJS     = lapack_demo_svd.o mod_file_handling.o mod_matrix_reader.o mod_matrix_writer.o mod_set_precision.o mod_svd_parameters.o
# MODS     = mod_file_handling.f08 mod_matrix_reader.f08 mod_matrix_writer.f08 mod_set_precision.f08 mod_svd_parameters.f08
# MOD_OBJS = mod_file_handling.o mod_matrix_reader.o mod_matrix_writer.o mod_set_precision.o mod_svd_parameters.o
# rditldmt@ITL-DTOPA-MP:bravo $
# rditldmt@ITL-DTOPA-MP:bravo $ make
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_file_handling.o mod_file_handling.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_set_precision.o mod_set_precision.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_svd_parameters.o mod_svd_parameters.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_matrix_reader.o mod_matrix_reader.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o mod_matrix_writer.o mod_matrix_writer.f08
# gfortran -c -g -ffpe-trap=denormal -fbacktrace -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -Wuse-without-only -o lapack_demo_svd.o lapack_demo_svd.f08
# gfortran -g -framework Accelerate -o lapack_demo_svd lapack_demo_svd.o mod_file_handling.o mod_matrix_reader.o mod_matrix_writer.o mod_set_precision.o mod_svd_parameters.o
