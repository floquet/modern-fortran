all: forsee

forsee: forsee.f
	gfortran ${gflags} -fno-align-commons -o forsee forsee.f

clean:
	rm -f forsee
