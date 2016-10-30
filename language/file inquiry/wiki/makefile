# Defining variables
objects = mod_file_inquire.o file_inquire.o
f08comp = /usr/local/bin/gfortran
cflags = -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5
switch = -Og

# Makefile
a.out: $(objects)
	$(f08comp) -o file_inquire.out $(switch) $(objects)
mod_file_inquire.mod: mod_file_inquire.o mod_file_inquire.f08
	$(f08comp) $(cflags) $(switch) mod_file_inquire.f08
%.o: %.f08
	$(f08comp) $(cflags) $(switch) $<

# Cleaning everything
clean :
	rm -f $(objects) *.mod *.o

# dan-topas-pro-2:wiki rditldmt$ date
# Fri Mar 18 18:46:02 CDT 2016
# dan-topas-pro-2:wiki rditldmt$ pwd
# /Users/rditldmt/Box Sync/fortran/demos/file inquiry/wiki
# dan-topas-pro-2:wiki rditldmt$ make
# /usr/local/bin/gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og mod_file_inquire.f08
# /usr/local/bin/gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og file_inquire.f08
# /usr/local/bin/gfortran -o file_inquire.out -Og mod_file_inquire.o file_inquire.o
