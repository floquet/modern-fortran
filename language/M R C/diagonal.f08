!  Modern Fortran Explained
!  Metcalf, Reid, Cohen
!  Figure 20.5 Diagonal of contiguous matrix
!  p. 364

program diagonal

    implicit none

    integer, parameter :: m = 3, n = 4
    integer            :: col, row
    real, target       :: a ( m, n )
    real, pointer      :: a_flat ( : ), a_diag ( : )

        a_flat ( 1 : n * m ) => a
        a_diag               => a_flat ( :: m + 1 )

        do col = 1, n
            do row = 1, m
                a ( row, col ) = 10 * row + col
                print *, 'a ( ', row, ', ', col, ' ) = ', a ( row, col )
            end do
        end do
        print *, 'a = ', a

        print *, 'diagonal = ', a_diag

end program diagonal

! dan-topas-pro-2:M R C rditldmt$ date
! Thu Oct  1 13:15:13 CDT 2015
! dan-topas-pro-2:M R C rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/M R C
! dan-topas-pro-2:M R C rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 diagonal.f08
! dan-topas-pro-2:M R C rditldmt$ ./a.out
!  a (            1 ,            1  ) =    11.0000000
!  a (            2 ,            1  ) =    21.0000000
!  a (            3 ,            1  ) =    31.0000000
!  a (            1 ,            2  ) =    12.0000000
!  a (            2 ,            2  ) =    22.0000000
!  a (            3 ,            2  ) =    32.0000000
!  a (            1 ,            3  ) =    13.0000000
!  a (            2 ,            3  ) =    23.0000000
!  a (            3 ,            3  ) =    33.0000000
!  a (            1 ,            4  ) =    14.0000000
!  a (            2 ,            4  ) =    24.0000000
!  a (            3 ,            4  ) =    34.0000000
!  a =    11.0000000       21.0000000       31.0000000       12.0000000       22.0000000       32.0000000       13.0000000       23.0000000       33.0000000       14.0000000       24.0000000       34.0000000
!  diagonal =    11.0000000       22.0000000       33.0000000
