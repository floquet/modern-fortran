
fxeon_base:
	ftn -O3 -xhost -openmp -o fsaxpy_xeon_base fsaxpy_base.f90

fxeon_opt:
	ftn -align array64byte -O3 -xhost -openmp -o fsaxpy_xeon_opt fsaxpy_opt.f90

fmic_offl_base:
	ftn -dynamic  -O3 -xhost -openmp -align array64byte -o fsaxpy_offl_base fsaxpy_offl_base.f90

fmic_offl_opt1:
	ftn -dynamic -O3 -xhost -align array64byte -openmp -o fsaxpy_offl_opt1 fsaxpy_offl_opt1.f90

fmic_offl_opt2:
	ftn -dynamic  -V -O3 -xhost -openmp -o fsaxpy_offl_opt2 fsaxpy_offl_opt2.f90

fmic_offl_opt3:
	ftn -dynamic -O3 -xhost -openmp -o fsaxpy_offl_opt3 fsaxpy_offl_opt3.f90

clean:
	rm -f fsaxpy_xeon_base saxpy_xeon_opt fsaxpy_offl_base
	rm -f fsaxpy_offl_opt1 fsaxpy_offl_opt2 fsaxpy_offl_opt3
