# Find all source files, create a list of corresponding object files
PROGRAM = test_rotations_module

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
mod_mSetPrecision.o        :
mod_rotation_routines.o    : mod_set_precision.o

# rditldmt@ITL-DTOPA-MP:rotations $ date
# Tue Aug  9 16:32:32 CDT 2016
# rditldmt@ITL-DTOPA-MP:rotations $ pwd
# /Users/rditldmt/hpc/fortran/RA/rotations
# rditldmt@ITL-DTOPA-MP:rotations $ make debug
# PROGRAM  = test_rotations_module
# PRG_OBJ  = test_rotations_module.o
# SRCS     = mod_rotation_routines.f08 mod_set_precision.f08 test_rotations_module.f08
# OBJS     = mod_rotation_routines.o mod_set_precision.o test_rotations_module.o
# MODS     = mod_rotation_routines.f08 mod_set_precision.f08
# MOD_OBJS = mod_rotation_routines.o mod_set_precision.o
