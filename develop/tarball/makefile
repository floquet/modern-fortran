# Find all source files, create a list of corresponding object files
PROGRAM = lucas_idea

EXT = f08
# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.$(EXT))
OBJS=$(patsubst %.$(EXT),%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.$(EXT))
MOD_OBJS=$(patsubst %.$(EXT),%.o,$(MODS))

# Compiler, Linker settings
FC = mpif90
FLFLAGS = -g
FCFLAGS = -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -O -pedantic -fcheck=bounds -fmax-errors=5
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

# rditldmt@ITL-DTOPA-MP:tarball $ date
# Tue Apr 26 16:18:44 CDT 2016
# rditldmt@ITL-DTOPA-MP:tarball $ pwd
# /Users/rditldmt/hpc/fortran/develop/tarball
# rditldmt@ITL-DTOPA-MP:tarball $ make debug
# PROGRAM  = lucas_idea
# PRG_OBJ  = lucas_idea.o
# SRCS     = lucas_idea.f08 mod_file_handling.f08
# OBJS     = lucas_idea.o mod_file_handling.o
# MODS     = mod_file_handling.f08
# MOD_OBJS = mod_file_handling.o
# rditldmt@ITL-DTOPA-MP:tarball $ make
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -O -pedantic -fcheck=bounds -fmax-errors=5 -o mod_file_handling.o mod_file_handling.f08
# mpif90 -c -g -Wall -Waliasing -Wconversion-extra -Wextra -Wsurprising -Wimplicit-procedure -Wintrinsics-std -Wuse-without-only -O -pedantic -fcheck=bounds -fmax-errors=5 -o lucas_idea.o lucas_idea.f08
# mpif90 -g -o lucas_idea lucas_idea.o mod_file_handling.o
