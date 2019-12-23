!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! http://www.dursi.ca/coarray-fortran-goes-mainstream-gcc-5-1/

program coarray2

    use iso_fortran_env
    implicit none

    integer :: me, right, k, status
    integer, dimension ( 3 ), codimension [ * ] :: a

    character ( * ), parameter :: c_options = compiler_options( )
    character ( * ), parameter :: c_version = compiler_version( )
    character ( len = 255 )    :: host = " ", cmd = " "

        ! queries
        call hostnm      ( host, status )
        call get_command ( cmd )

        ! write identifiers
        write ( *, '( /, "host system       = ", A    )' ) trim( host )
        write ( *, '(    "compiler version  = ", A    )' ) c_version
        write ( *, '(    "compiler options  = ", A    )' ) trim( c_options )
        write ( *, '(    "execution command = ", A, / )' ) trim( cmd )

        me = this_image( )

        right = me + 1
        if ( right > num_images( ) ) right = 1

        a ( : ) = [ ( me ** k, k = 1, 3 ) ]

        sync all

        print *, "Image ", me, " has a(2) = ", a( 2 )[ me ], "; neighbour has ", a( 2 )[ right ]

      stop 'successful program execution for coarray2'

end program coarray2

! gfortran: note: valid arguments to ‘-fcoarray=’ are: lib none single

! dan-topas-pro-2:demos rditldmt$ date
! Mon Aug 31 17:12:39 CDT 2015
! dan-topas-pro-2:demos rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/coarray/dursi/demos
! dan-topas-pro-2:demos rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=single coarray2.f90
! dan-topas-pro-2:demos rditldmt$ ./a.out
!
! host system       = dan-topas-pro-2.erdc.dren.mil
! compiler version  = GCC version 5.1.0
! compiler options  = -fPIC -mmacosx-version-min=10.9.4 -mtune=core2 -Og -Wall -Wextra -Wconversion -Wpedantic -fcheck=bounds -fmax-errors=5 -fcoarray=single
! execution command = ./a.out
!
!  Image            1  has a(2) =            1 ; neighbour has            1
! STOP successful program execution for coarray2
! dan-topas-pro-2:demos rditldmt$


!     gfortran: note: valid arguments to ‘-fcoarray=’ are: lib none single
!     Muntz-Szasz:demos dantopa$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 -fcoarray=lib coarray2.f90
!     Undefined symbols for architecture x86_64:
!       "__gfortran_caf_get", referenced from:
!           _MAIN__ in ccaNBAsc.o
!       "__gfortran_caf_init", referenced from:
!           _main in ccaNBAsc.o
!       "__gfortran_caf_num_images", referenced from:
!           _MAIN__ in ccaNBAsc.o
!       "__gfortran_caf_register", referenced from:
!           __caf_init.1.3417 in ccaNBAsc.o
!       "__gfortran_caf_sync_all", referenced from:
!           _MAIN__ in ccaNBAsc.o
!       "__gfortran_caf_this_image", referenced from:
!           _MAIN__ in ccaNBAsc.o
!     ld: symbol(s) not found for architecture x86_64
!     collect2: error: ld returned 1 exit status

