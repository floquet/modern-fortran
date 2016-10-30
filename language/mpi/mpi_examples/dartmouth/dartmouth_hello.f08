!  https://www.dartmouth.edu/~rc/classes/intro_mpi/compiling_mpi_ex.html#top
   program hello
   include 'mpif.h'
   integer rank, size, ierror, tag, status(MPI_STATUS_SIZE)

   call MPI_INIT(ierror)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
   call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
   print*, 'node', rank, ': Hello world'
   call MPI_FINALIZE(ierror)
   end

! Muntz-Szasz:dartmouth dantopa$ date
! Sun Mar 20 13:50:27 CDT 2016
! Muntz-Szasz:dartmouth dantopa$ pwd
! /Users/dantopa/Box Sync/fortran/demos/MPI/mpi examples/dartmouth
! Muntz-Szasz:dartmouth dantopa$ mpif90 -o dartmouth_hello dartmouth_hello.f08
! Muntz-Szasz:dartmouth dantopa$ mpirun -np 8 ./dartmouth_hello
!  node           0 : Hello world
!  node           3 : Hello world
!  node           4 : Hello world
!  node           5 : Hello world
!  node           6 : Hello world
!  node           7 : Hello world
!  node           1 : Hello world
!  node           2 : Hello world
! Muntz-Szasz:dartmouth dantopa$ mpif90 -compile_info
! /opt/local/bin/gfortran-mp-4.9 -pipe -m64 -Wl,-headerpad_max_install_names -arch x86_64 -Wl,-flat_namespace -Wl,-commons,use_dylibs -I/opt/local/include/mpich-mp -I/opt/local/include/mpich-mp -L/opt/local/lib/mpich-mp -lmpifort -lmpi -lpmpi
! Muntz-Szasz:dartmouth dantopa$ mpif90 -v
! mpifort for MPICH version 3.2
! Using built-in specs.
! COLLECT_GCC=/opt/local/bin/gfortran-mp-4.9
! COLLECT_LTO_WRAPPER=/opt/local/libexec/gcc/x86_64-apple-darwin15/4.9.3/lto-wrapper
! Target: x86_64-apple-darwin15
! Configured with: /opt/local/var/macports/build/_opt_local_var_macports_sources_rsync.macports.org_release_tarballs_ports_lang_gcc49/gcc49/work/gcc-4.9.3/configure --prefix=/opt/local --build=x86_64-apple-darwin15 --enable-languages=c,c++,objc,obj-c++,lto,fortran,java --libdir=/opt/local/lib/gcc49 --includedir=/opt/local/include/gcc49 --infodir=/opt/local/share/info --mandir=/opt/local/share/man --datarootdir=/opt/local/share/gcc-4.9 --with-local-prefix=/opt/local --with-system-zlib --disable-nls --program-suffix=-mp-4.9 --with-gxx-include-dir=/opt/local/include/gcc49/c++/ --with-gmp=/opt/local --with-mpfr=/opt/local --with-mpc=/opt/local --with-isl=/opt/local --disable-isl-version-check --with-cloog=/opt/local --disable-cloog-version-check --enable-stage1-checking --disable-multilib --enable-lto --enable-libstdcxx-time --with-as=/opt/local/bin/as --with-ld=/opt/local/bin/ld --with-ar=/opt/local/bin/ar --with-bugurl=https://trac.macports.org/newticket --with-pkgversion='MacPorts gcc49 4.9.3_0' --with-build-config=bootstrap-debug
! Thread model: posix
! gcc version 4.9.3 (MacPorts gcc49 4.9.3_0)
