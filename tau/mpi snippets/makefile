# Find all source files, create a list of corresponding object files
PROGRAM = pi_constant_time

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
FC = mpif90
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
#mod_accumulator.o           : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_timer_clock.o mod_timer_CPU.o mod_unit_values.o

#  09:50 ITL-DTOPA-MP rditldmt $ date
# Thu Apr 14 09:50:40 CDT 2016
#  09:50 ITL-DTOPA-MP rditldmt $ pwd
# /Users/rditldmt/Documents/singles/pi_constant_time
#  09:50 ITL-DTOPA-MP rditldmt $ make debug
# PROGRAM  = pi_constant_time
# PRG_OBJ  = pi_constant_time.o
# SRCS     = mod_file_handling.f08 pi_constant_time.f08
# OBJS     = mod_file_handling.o pi_constant_time.o
# MODS     = mod_file_handling.f08
# MOD_OBJS = mod_file_handling.o
#  09:50 ITL-DTOPA-MP rditldmt $ make
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f08
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -o pi_constant_time.o pi_constant_time.f08
# pi_constant_time.f08:24:69:
#
#          call MPI_INIT ( ierr )               ! initialize mpi runtime
#                                                                      1
# Warning: Procedure 'mpi_init' called at (1) is not explicitly declared [-Wimplicit-procedure]
# pi_constant_time.f08:26:75:
#
#          call MPI_COMM_RANK ( MPI_COMM_WORLD, rank,  ierr ) ! PE's grab rank
#                                                                            1
# Warning: Procedure 'mpi_comm_rank' called at (1) is not explicitly declared [-Wimplicit-procedure]
# pi_constant_time.f08:27:75:
#
#          call MPI_COMM_SIZE ( MPI_COMM_WORLD, numPE, ierr ) ! number of PE's
#                                                                            1
# Warning: Procedure 'mpi_comm_size' called at (1) is not explicitly declared [-Wimplicit-procedure]
# pi_constant_time.f08:39:80:
#
#              call MPI_BCAST ( numIntDomain, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr )
#                                                                                 1
# Warning: Procedure 'mpi_bcast' called at (1) is not explicitly declared [-Wimplicit-procedure]
# pi_constant_time.f08:52:88:
#
#              call MPI_REDUCE ( dpi, pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD, ierr )
#                                                                                         1
# Warning: Procedure 'mpi_reduce' called at (1) is not explicitly declared [-Wimplicit-procedure]
# pi_constant_time.f08:57:25:
#
#                  io_out = safeopen_writeappend ( trim( host_name ) // file_results )
#                          1
# Warning: Conversion from INTEGER(4) to INTEGER(8) at (1) [-Wconversion-extra]
# pi_constant_time.f08:64:58:
#
#          call MPI_FINALIZE ( ierr )  ! clean up and go home
#                                                           1
# Warning: Procedure 'mpi_finalize' called at (1) is not explicitly declared [-Wimplicit-procedure]
# mpif90 -g -o pi_constant_time mod_file_handling.o pi_constant_time.o
