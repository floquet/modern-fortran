# makefile
objects = a.o b.o c.o transporter.o
Comp = /Applications/Absoft10.1/bin/f90 # Absoft compiler
#Comp = /usr/bin/ifort # Intel compiler
FFLAGS = -g
#FFLAGS = -O
abc : $(objects)
$(Comp) -o abc $(objects)
%.o : %.f90
$(Comp) ${FFLAGS} -c $<
