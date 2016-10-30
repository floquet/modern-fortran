# Defining variables
objects = mod_format_descriptors.o mod_grain.o mod_mathematica_results.o mod_matrix_writer.o mod_parameters_simulation.o mod_read_LAMMPS.o mod_shared.o secundus.o
f08 = gfortran
cflags = -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5
switch = -Og
# makefile
a.out: $(objects)
	$(f08) -o a.out $(switch) $(objects)
# modules
mod_format_descriptors.mod: mod_format_descriptors.o mod_format_descriptors.f08
	$(f08) $(cflags) $(switch) mod_format_descriptors.f08
mod_grain.mod: mod_grain.o mod_grain.f08
	$(f08) $(cflags) $(switch) mod_grain.f08
mod_mathematica_results.mod: mod_mathematica_results.o mod_mathematica_results.f08
	$(f08) $(cflags) $(switch) mod_mathematica_results.f08
mod_matrix_writer.mod: mod_matrix_writer.o mod_matrix_writer.f08
	$(f08) $(cflags) $(switch) mod_matrix_writer.f08
mod_parameters_simulation.mod: mod_parameters_simulation.o mod_parameters_simulation.f08
	$(f08) $(cflags) $(switch) mod_parameters_simulation.f08
mod_read_LAMMPS.mod: mod_read_LAMMPS.o mod_read_LAMMPS.f08
	$(f08) $(cflags) $(switch) mod_read_LAMMPS.f08
mod_shared.mod: mod_shared.o mod_shared.f08
	$(f08) $(cflags) $(switch) mod_shared.f08
# executable
secundus.o: mod_format_descriptors.o mod_grain.o mod_mathematica_results.o mod_matrix_writer.o mod_parameters_simulation.o mod_read_LAMMPS.o mod_shared.o secundus.f08
	$(f08) $(cflags) $(switch) secundus.f08
%.o: %.f08
	$(f08) $(cflags) $(switch) $<
clean:
	rm -f $(objects) *.mod
