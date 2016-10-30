F90 = /usr/bin/nagfor

##########################
# Object Files for build #
##########################

OBJS = \
util.o \

alpha : $(OBJS)
	 ${F90}  -o $@ $(OBJS)

#######################################
# Object dependencies and compilation #
#######################################
util.o : /Users/dantopa/Dropbox/Fortran/alpha/util.f90
	$(F90) -c $(FFLAGS) $(INCLUDES) -o $@ /Users/dantopa/Dropbox/Fortran/alpha/util.f90

