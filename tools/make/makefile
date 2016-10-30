# https://stackoverflow.com/questions/5871780/creating-a-fortran-makefile
# compiler
FC      := /usr/local/bin/gfortran

# compile flags
FCFLAGS = -fbounds-check
# link flags
FLFLAGS =

# source files and objects
SRCS = $(patsubst %.f08, %.o, $(wildcard *.f08))
#       $(patsubst %.h, %.mod, $(wildcard *.h))

# program name
PROGRAM = test_make

all: $(PROGRAM)

$(PROGRAM): $(SRCS)
    $(FC) $(FLFLAGS) -o $@ $^

%.o: %.f08
    $(FC) $(FCFLAGS) -o $@ $<

# %.mod: %.h
# $(FC) $(FCFLAGS) -o $@ $<

clean:
    rm -f *.o *.mod
