# https://stackoverflow.com/questions/35234003/how-to-create-a-makefile-for-a-fortran-program-using-modules
# https://stackoverflow.com/questions/5871780/creating-a-fortran-makefile
# Find all source files, create a list of corresponding object files
PROGRAM = prime_density

SRCS=$(wildcard *.f08)
OBJS=$(patsubst %.f08,%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.f08)
MOD_OBJS=$(patsubst %.f08,%.o,$(MODS))

# Compiler/Linker settings
# cafrun -np 22 ./a.out
FC = caf
FLFLAGS = -g
FCFLAGS = -c ${cflags}
PRG_OBJ = $(PROGRAM).o

# Clean the suffixes
.SUFFIXES:

# Set the suffixes we are interested in
.SUFFIXES: .f08 .o

# make without parameters will make first target found.
default : $(PROGRAM)

# Compiler steps for all objects
$(OBJS) : %.o : %.f08
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
	rm -rf *.mod *.smod *.o *.dSYM	

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# Blocks and allocations depends on shared
# mod_unit_values, mod_timer_CPU have no dependencies
# mod_accumulator.o           : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_timer_clock.o mod_timer_CPU.o mod_unit_values.o

#  15:54 ITL-DTOPA-MP rditldmt $ make debug
# PROGRAM  = prime_density
# PRG_OBJ  = prime_density.o
# SRCS     = mod_Prime_Tools.f08 prime_density.f08
# OBJS     = mod_Prime_Tools.o prime_density.o
# MODS     = mod_Prime_Tools.f08
# MOD_OBJS = mod_Prime_Tools.o
#  15:54 ITL-DTOPA-MP rditldmt $ make
# caf -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib -o mod_Prime_Tools.o mod_Prime_Tools.f08
# caf -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib -o prime_density.o prime_density.f08
# caf -g -o prime_density mod_Prime_Tools.o prime_density.o
#  15:54 ITL-DTOPA-MP rditldmt $
#  15:54 ITL-DTOPA-MP rditldmt $ date
# Fri Apr 15 15:54:40 CDT 2016
#  15:54 ITL-DTOPA-MP rditldmt $ pwd
# /Users/rditldmt/Box Sync/fortran/coarray/demos/vm_primes
