# Defining variables
objects = mod\ file\ inquire.o file_inquire.o
f08comp = /usr/local/bin/gfortran
cflags = -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5
switch = -Og

# Makefile
a.out: $(objects)
	$(f08comp) -o a.out $(switch) $(objects)
mod\ file\ inquire.mod: mod\ file\ inquire.o mod\ file\ inquire.f08
	$(f08comp) $(cflags) $(switch) mod\ file\ inquire.f08
mod\ file\ inquire.o: mod\ file\ inquire.f08
	$(f08comp) $(cflags) $(switch) mod\ file\ inquire.f08
file_inquire.o: mod\ file\ inquire.mod file_inquire.f08
	$(f08comp) $(cflags) $(switch) file_inquire.f08
%.o: %.f08
	$(f08comp) $(cflags) $(switch) $<

# Cleaning everything
clean :
	rm -f $(objects) *.mod *.o

# dantopa@Muntz-Szasz:~/Box Sync/fortran/demos/inquire$ date
# Sun Feb  7 20:13:18 CST 2016
# dantopa@Muntz-Szasz:~/Box Sync/fortran/demos/inquire$ pwd
# /Users/dantopa/Box Sync/fortran/demos/inquire
# dantopa@Muntz-Szasz:~/Box Sync/fortran/demos/inquire$ make
# /usr/local/bin/gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og mod\ file\ inquire.f08
# /usr/local/bin/gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og file_inquire.f08
# /usr/local/bin/gfortran -o a.out -Og mod\ file\ inquire.o file_inquire.o
