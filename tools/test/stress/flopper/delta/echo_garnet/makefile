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
FC = ftn 
FLFLAGS = -g
FCFLAGS = -c 
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
 mod_test_suite.o           : mod_timer_CPU.o mod_timer_clock.o

# dantopa@thunder09:echo_thunder $ pwd
# /home/dantopa/hpc/fortran/tools/test/stress/flopper/delta/echo_thunder
# dantopa@thunder09:echo_thunder $ date
# Sun Jun 12 18:30:50 EDT 2016
# dantopa@thunder09:echo_thunder $ git add .
# dantopa@thunder09:echo_thunder $ git pull
# X11 forwarding request failed on channel 0
# Already up-to-date.
# dantopa@thunder09:echo_thunder $ git commit -am 'thunder finally running'
# [master a118899] thunder finally running
#  3 files changed, 164 insertions(+), 61 deletions(-)
#  rewrite tools/test/stress/flopper/delta/echo_thunder/flops (65%)
#  rename tools/test/stress/flopper/delta/echo_thunder/{mod_mpif_sgi.f08 => xmod_mpif_sgi.f08} (100%)
# dantopa@thunder09:echo_thunder $ git push
# X11 forwarding request failed on channel 0
# Counting objects: 26, done.
# Delta compression using up to 56 threads.
# Compressing objects: 100% (20/20), done.
# Writing objects: 100% (20/20), 51.89 KiB, done.
# Total 20 (delta 15), reused 0 (delta 0)
# To git@bitbucket.org:dantopa/hpc_fortran.git
#    b293575..a118899  master -> master
# dantopa@thunder09:echo_thunder $ make debug
# PROGRAM  = flops
# PRG_OBJ  = flops.o
# SRCS     = flops.f08 mod_file_handling.f08 mod_harvest_blade.f08 mod_test_suite.f08 mod_timer_CPU.f08 mod_timer_clock.f08 xmod_mpif_sgi.f08
# OBJS     = flops.o mod_file_handling.o mod_harvest_blade.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o xmod_mpif_sgi.o
# MODS     = mod_file_handling.f08 mod_harvest_blade.f08 mod_test_suite.f08 mod_timer_CPU.f08 mod_timer_clock.f08
# MOD_OBJS = mod_file_handling.o mod_harvest_blade.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o

# over 1000 lines of warnings...
# Warning: Unused parameter 'mpi_win_unified' declared at (1)
# Warning: Unused parameter 'mpi_win_unified' declared at (1)
# mpif.h:210.35:
#     Included at flops.f08:9:
# 
#         integer MPI_WTIME_IS_GLOBAL
#                                    1
# Warning: Unused parameter 'mpi_wtime_is_global' declared at (1)
# mpif.h:458.31:
#     Included at flops.f08:9:
# 
#       INTEGER MPIO_REQUEST_NULL
#                                1
# Warning: Unused parameter 'mpio_request_null' declared at (1)
# mpif90  -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -O -pedantic -fcheck=bounds -fmax-errors=5 -o xmod_mpif_sgi.o xmod_mpif_sgi.f08
# mpif90  -g -o flops flops.o mod_file_handling.o mod_harvest_blade.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o xmod_mpif_sgi.o
# mpif.h:210.35:
#     Included at flops.f08:9:
# 
#         integer MPI_WTIME_IS_GLOBAL
#                                    1
# Warning: Unused parameter 'mpi_wtime_is_global' declared at (1)
# mpif.h:458.31:
#     Included at flops.f08:9:
# 
#       INTEGER MPIO_REQUEST_NULL
#                                1
# Warning: Unused parameter 'mpio_request_null' declared at (1)
# mpif90  -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -O -pedantic -fcheck=bounds -fmax-errors=5 -o xmod_mpif_sgi.o xmod_mpif_sgi.f08
# mpif90  -g -o flops flops.o mod_file_handling.o mod_harvest_blade.o mod_test_suite.o mod_timer_CPU.o mod_timer_clock.o xmod_mpif_sgi.o

