# Start of the makefile
# Defining variables
objects = global.o main.o function1.o subroutine1.o
f08comp = gfortran
cflags = -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5
switch = -Og
# Makefile
a.out: $(objects)
	$(f08comp) -o a.out $(switch) $(objects)
global.mod: global.o global.f08
	$(f08comp) $(cflags) $(switch) global.f08
global.o: global.f08
	$(f08comp) $(cflags) $(switch) global.f08
main.o: global.mod main.f08
	$(f08comp) $(cflags) $(switch) main.f08
function1.o: global.mod function1.f08
	$(f08comp) $(cflags) $(switch) function1.f08
%.o: %.f08
	$(f08comp) $(cflags) $(switch) $<
# Cleaning everything
clean :
	rm -f $(objects) *.mod
# End of the makefile

# http://genius2k.is-programmer.com/posts/40301.html

# dan-topas-pro-2:genius2k rditldmt$ date
# Fri Jan 29 13:13:46 CST 2016
# dan-topas-pro-2:genius2k rditldmt$ pwd
# /Users/rditldmt/Box Sync/fortran/demos/make/genius2k
# dan-topas-pro-2:genius2k rditldmt$ make
# gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og global.f08
# gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og global.f08
# gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og main.f08
# gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og function1.f08
# gfortran -c -Wall -Wextra -Wconversion -pedantic -fcheck=bounds -fmax-errors=5 -Og subroutine1.f08
# gfortran -o a.out -Og global.o main.o function1.o subroutine1.o
# dan-topas-pro-2:genius2k rditldmt$ aa
# y = 16.0000000
#
# subroutine1 = 17.0000000
#
# STOP Execution complete for main.f08
# dan-topas-pro-2:genius2k rditldmt$ make clean
# rm -f global.o main.o function1.o subroutine1.o mymodule.mod
# dan-topas-pro-2:genius2k rditldmt$ lss
# total 80
# drwxr-xr-x  11 rditldmt  ERDC\Domain Users   374B Jan 29 13:14 .
# drwxr-xr-x   8 rditldmt  ERDC\Domain Users   272B Jan 29 10:01 ..
# -rwxr-xr-x   1 rditldmt  ERDC\Domain Users   8.9K Jan 29 13:13 a.out
# -rw-r--r--   1 rditldmt  ERDC\Domain Users   104B Jan 29 10:25 function1.f08
# -rw-r--r--   1 rditldmt  ERDC\Domain Users    92B Jan 29 10:05 global.f08
# -rw-r--r--   1 rditldmt  ERDC\Domain Users   389B Jan 29 10:28 main.f08
# -rw-r--r--   1 rditldmt  ERDC\Domain Users   1.2K Jan 29 13:12 makefile
# -rw-r--r--   1 rditldmt  ERDC\Domain Users   1.2K Jan 29 10:38 makefile01
# -rw-r--r--   1 rditldmt  ERDC\Domain Users   1.2K Jan 29 12:58 makefile02
# drwxr-xr-x  13 rditldmt  ERDC\Domain Users   442B Jan 29 10:55 subdir
# -rw-r--r--   1 rditldmt  ERDC\Domain Users   161B Jan 29 09:55 subroutine1.f08
