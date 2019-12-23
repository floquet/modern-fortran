# Find all source files, create a list of corresponding object files
PROGRAM = lucas_idea

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
FC = ${make_fc}
FLFLAGS = -g
FCFLAGS = -c ${make_fcflags}
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
#mod_accumulator.o           : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_timer_clock.o mod_timer_CPU.o mod_unit_values.o

#  15:19 ITL-DTOPA-MP rditldmt $ make
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o lucas_idea.o lucas_idea.f08
# mpif90 -g -o lucas_idea lucas_idea.o
#  15:19 ITL-DTOPA-MP rditldmt $ mpirun -np 4 ./lucas_idea
#   numIntDomain = 1441440000, j = 3
#
# After MPI_BCAST of 1441440000 as MPI_INT, ierr = 0.
# MPI_INT:: rank = 0, numIntDomain + rank = 1441440000 (should be 1441440000 + 0)
# MPI_INT:: rank = 0, numIntDomain + rank - rank = 0 (should be 0)
#
# After MPI_BCAST of 1441440000 as MPI_INTEGER8, ierr = 0.
# MPI_INTEGER8:: rank = 0, numIntDomain + rank = 1441440000 (should be 1441440000 + 0)
# MPI_INTEGER8:: rank = 0, numIntDomain + rank - rank = 0 (should be 0)
#
# After MPI_BCAST of 1441440000 as MPI_INT, ierr = 0.
# MPI_INT:: rank = 2, numIntDomain + rank = 1441440002 (should be 1441440000 + 2)
# After MPI_BCAST of 1441440000 as MPI_INT, ierr = 0.
# MPI_INT:: rank = 3, numIntDomain + rank = 1441440003 (should be 1441440000 + 3)
# MPI_INT:: rank = 3, numIntDomain + rank - rank = 3 (should be 3)
#
# MPI_INT:: rank = 2, numIntDomain + rank - rank = 2 (should be 2)
#
# After MPI_BCAST of 1441440000 as MPI_INTEGER8, ierr = 0.
# MPI_INTEGER8:: rank = 2, numIntDomain + rank = 1441440002 (should be 1441440000 + 2)
# MPI_INTEGER8:: rank = 2, numIntDomain + rank - rank = 2 (should be 2)
#
# After MPI_BCAST of 1441440000 as MPI_INT, ierr = 0.
# MPI_INT:: rank = 1, numIntDomain + rank = 1441440001 (should be 1441440000 + 1)
# MPI_INT:: rank = 1, numIntDomain + rank - rank = 1 (should be 1)
#
# After MPI_BCAST of 1441440000 as MPI_INTEGER8, ierr = 0.
# After MPI_BCAST of 1441440000 as MPI_INTEGER8, ierr = 0.
# MPI_INTEGER8:: rank = 3, numIntDomain + rank = 1441440003 (should be 1441440000 + 3)
# MPI_INTEGER8:: rank = 3, numIntDomain + rank - rank = 3 (should be 3)
#
# MPI_INTEGER8:: rank = 1, numIntDomain + rank = 1441440001 (should be 1441440000 + 1)
# MPI_INTEGER8:: rank = 1, numIntDomain + rank - rank = 1 (should be 1)
#
# * numIntDomain = 14414400000, j = 4
#
# After MPI_BCAST of 14414400000 as MPI_INT, ierr = 0.
# MPI_INT:: rank = 0, numIntDomain + rank = 14414400000 (should be 14414400000 + 0)
# MPI_INT:: rank = 0, numIntDomain + rank - rank = 0 (should be 0)
#
# After MPI_BCAST of 14414400000 as MPI_INTEGER8, ierr = 0.
# MPI_INTEGER8:: rank = 0, numIntDomain + rank = 14414400000 (should be 14414400000 + 0)
# MPI_INTEGER8:: rank = 0, numIntDomain + rank - rank = 0 (should be 0)
#
# After MPI_BCAST of 1529498112 as MPI_INT, ierr = 0.
# MPI_INT:: rank = 2, numIntDomain + rank = 1529498114 (should be 1529498112 + 2)
# MPI_INT:: rank = 2, numIntDomain + rank - rank = 2 (should be 2)
#
# After MPI_BCAST of 14414400000 as MPI_INTEGER8, ierr = 0.
# After MPI_BCAST of 1529498112 as MPI_INT, ierr = 0.
# MPI_INT:: rank = 3, numIntDomain + rank = 1529498115 (should be 1529498112 + 3)
# MPI_INT:: rank = 3, numIntDomain + rank - rank = 3 (should be 3)
#
# After MPI_BCAST of 1529498112 as MPI_INT, ierr = 0.
# MPI_INT:: rank = 1, numIntDomain + rank = 1529498113 (should be 1529498112 + 1)
# MPI_INT:: rank = 1, numIntDomain + rank - rank = 1 (should be 1)
#
# After MPI_BCAST of 14414400000 as MPI_INTEGER8, ierr = 0.
# MPI_INTEGER8:: rank = 1, numIntDomain + rank = 1529498113 (should be 14414400000 + 1)
# MPI_INTEGER8:: rank = 1, numIntDomain + rank - rank = 1 (should be 1)
#
# MPI_INTEGER8:: rank = 2, numIntDomain + rank = 1529498114 (should be 14414400000 + 2)
# MPI_INTEGER8:: rank = 2, numIntDomain + rank - rank = 2 (should be 2)
#
# After MPI_BCAST of 14414400000 as MPI_INTEGER8, ierr = 0.
# MPI_INTEGER8:: rank = 3, numIntDomain + rank = 1529498115 (should be 14414400000 + 3)
# MPI_INTEGER8:: rank = 3, numIntDomain + rank - rank = 3 (should be 3)
