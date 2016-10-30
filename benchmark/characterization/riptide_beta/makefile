# https://stackoverflow.com/questions/35234003/how-to-create-a-makefile-for-a-fortran-program-using-modules
# https://stackoverflow.com/questions/5871780/creating-a-fortran-makefile
# Find all source files, create a list of corresponding object files
SRCS=$(wildcard *.f08)
OBJS=$(patsubst %.f08,%.o,$(SRCS))

# Ditto for mods (They will be in both lists)
MODS=$(wildcard mod*.f08)
MOD_OBJS=$(patsubst %.f08,%.o,$(MODS))

# Compiler/Linker settings
FC = gfortran
FLFLAGS = -g
<<<<<<< HEAD
FCFLAGS = -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5
=======
FCFLAGS = -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5
>>>>>>> d8754854d237c2124240287e5483133b2c6316e1
PROGRAM = characterize
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
<<<<<<< HEAD
	@echo "${bold}PROGRAM${normal}  = $(PROGRAM)"
	@echo "${bold}PRG_OBJ${normal}  = $(PRG_OBJ)"
	@echo "${bold}SRCS${normal}     = $(SRCS)"
	@echo "${bold}OBJS${normal}     = $(OBJS)"
	@echo "${bold}MODS${normal}     = $(MODS)"
	@echo "${bold}MOD_OBJS${normal} = $(MOD_OBJS)"
=======
	@echo "SRCS = $(SRCS)"
	@echo "OBJS = $(OBJS)"
	@echo "MODS = $(MODS)"
	@echo "MOD_OBJS = $(MOD_OBJS)"
	@echo "PROGRAM = $(PROGRAM)"
	@echo "PRG_OBJ = $(PRG_OBJ)"
>>>>>>> d8754854d237c2124240287e5483133b2c6316e1

clean:
	rm -rf $(OBJS) $(PROGRAM) $(patsubst %.o,%.mod,$(MOD_OBJS))
	rm -f *.mod

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(PRG_OBJ) : $(MOD_OBJS)

# Blocks and allocations depends on shared
<<<<<<< HEAD
# mod_unit_values, mod_timer_cpu have no dependencies
mod_accumulator.o           : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_timer_clock.o mod_timer_cpu.o mod_unit_values.o
=======
# mod_unit_values, mod_timer_CPU have no dependencies
mod_accumulator.o           : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_timer_clock.o mod_timer_CPU.o mod_unit_values.o
>>>>>>> d8754854d237c2124240287e5483133b2c6316e1
mod_allocations.o           : mod_precision_definitions.o mod_parameters.o mod_timer_clock.o mod_timer_cpu.o mod_shared.o mod_blocks.o mod_unit_values.o
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
<<<<<<< HEAD
mod_times.o                 : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_timer_cpu.o mod_timer_clock.o
mod_variable_types.o        : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_unit_values.o

# [dantopa@riptide04 riptide_alpha]$ date
# # Tue Apr 19 05:48:40 HST 2016
# # [dantopa@riptide04 riptide_alpha]$ pwd
# # /gpfs/home/dantopa/github/fortran/benchmark/characterization/riptide_alpha
# # [dantopa@riptide04 riptide_alpha]$ make debug
# # PROGRAM  = characterize
# # PRG_OBJ  = characterize.o
# # SRCS     = characterize.f08 mod_accumulator.f08 mod_allocations.f08 mod_blocks.f08 mod_character_encodings.f08 mod_constants.f08 mod_declaration_precision.f08 mod_epsilon.f08 mod_iso_variables.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_precision_request.f08 mod_queries.f08 mod_shared.f08 mod_timer_clock.f08 mod_timer_cpu.f08 mod_times.f08 mod_unit_values.f08 mod_variable_types.f08
# # OBJS     = characterize.o mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_clock.o mod_timer_cpu.o mod_times.o mod_unit_values.o mod_variable_types.o
# # MODS     = mod_accumulator.f08 mod_allocations.f08 mod_blocks.f08 mod_character_encodings.f08 mod_constants.f08 mod_declaration_precision.f08 mod_epsilon.f08 mod_iso_variables.f08 mod_parameters.f08 mod_precision_definitions.f08 mod_precision_request.f08 mod_queries.f08 mod_shared.f08 mod_timer_clock.f08 mod_timer_cpu.f08 mod_times.f08 mod_unit_values.f08 mod_variable_types.f08
# # MOD_OBJS = mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_clock.o mod_timer_cpu.o mod_times.o mod_unit_values.o mod_variable_types.o
# # [dantopa@riptide04 riptide_alpha]$ make
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_definitions.o mod_precision_definitions.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_parameters.o mod_parameters.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_shared.o mod_shared.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_blocks.o mod_blocks.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_timer_clock.o mod_timer_clock.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_timer_cpu.o mod_timer_cpu.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_unit_values.o mod_unit_values.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_accumulator.o mod_accumulator.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_allocations.o mod_allocations.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_character_encodings.o mod_character_encodings.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_constants.o mod_constants.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_declaration_precision.o mod_declaration_precision.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_epsilon.o mod_epsilon.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_iso_variables.o mod_iso_variables.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_precision_request.o mod_precision_request.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_queries.o mod_queries.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_times.o mod_times.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o mod_variable_types.o mod_variable_types.f08
# # gfortran -g -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -o characterize.o characterize.f08
# # gfortran -g -o characterize characterize.o mod_accumulator.o mod_allocations.o mod_blocks.o mod_character_encodings.o mod_constants.o mod_declaration_precision.o mod_epsilon.o mod_iso_variables.o mod_parameters.o mod_precision_definitions.o mod_precision_request.o mod_queries.o mod_shared.o mod_timer_clock.o mod_timer_cpu.o mod_times.o mod_unit_values.o mod_variable_types.o
#
=======
mod_times.o                 : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_timer_CPU.o mod_timer_clock.o
mod_variable_types.o        : mod_precision_definitions.o mod_blocks.o mod_shared.o mod_unit_values.o
>>>>>>> d8754854d237c2124240287e5483133b2c6316e1
