! https://takisword.wordpress.com/2012/08/04/legendre-polynomial-generation-in-fortran/
! Copyright (C) 2012 Yi Zhang

! Author: Yi Zhang <zhangy2@onid.orst.edu>
! Keywords: numerical, tools, polynomial

! This file is free software; you can redistribute it and/or modify
! it under the terms of the FreeBSD License. So essentially do
! whatever you want.

module legendre_poly

    implicit none

    type polycoef
        real, pointer :: p( : )
    end type polycoef

contains

    subroutine gen_legendre_poly( )
 ! q_n = p_(n-1) = 1/(n-1)*[(2n-3)xp_(n-2)-(n-2)p_(n-3)]
 !     = 1/(n-1)*[(2n-3)xq_(n-1)-(n-2)q_(n-2)]
    integer, parameter :: n = 11
    type ( polycoef )  :: a( n )
    integer            :: i, j

        do i = 1, n
            allocate( a( i ) % p( i ) )
            a( i ) % p = 0
        end do

        a( 1 ) % p( 1 ) = 1
        a( 2 ) % p       = [ 0, 1 ]
        write ( *, 100 ) 0
        write ( *, * ) a( 1 ) % p
        write ( *, 100 ) 1
        write ( *, * ) a( 2 ) % p

        do i = 3, n
            do j = 1, i - 2
                a( i ) % p( j ) = -( i - 2 ) * a( i - 2 ) % p( j )
            enddo
            do j = 1, i - 1
                a( i ) % p( j + 1 ) = a( i ) % p( j + 1 ) + ( 2 * i - 3 ) * a( i - 1 ) % p( j )
            enddo
            a( i ) % p = a( i ) % p / ( i - 1 )
            write ( *, 100 ) i - 1
            write ( *, * ) a( i ) % p
        enddo

        return
  100   format ( "m = ", g0 )

    end subroutine gen_legendre_poly

end module legendre_poly

program driver
    use legendre_poly
    implicit none
        call gen_legendre_poly( )
end program driver

! dan-topas-pro-2:myModules rditldmt$ date
! Tue Jan 19 09:33:23 CST 2016
! dan-topas-pro-2:myModules rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/projects/orthogonality/legendre functions/myModules
! dan-topas-pro-2:myModules rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 mod\ legendre_poly.f08
! dan-topas-pro-2:myModules rditldmt$ ./a.out
! m = 0
!    1.00000000
! m = 1
!    0.00000000       1.00000000
! m = 2
!  -0.500000000       0.00000000       1.50000000
! m = 3
!   -0.00000000      -1.50000000       0.00000000       2.50000000
! m = 4
!   0.375000000      -0.00000000      -3.75000000       0.00000000       4.37500000
! m = 5
!    0.00000000       1.87500000      -0.00000000      -8.75000000       0.00000000       7.87500000
! m = 6
!  -0.312500000       0.00000000       6.56250000      -0.00000000      -19.6875000       0.00000000       14.4375000
! m = 7
!   -0.00000000      -2.18750000       0.00000000       19.6875000      -0.00000000      -43.3125000       0.00000000       26.8125000
! m = 8
!   0.273437500      -0.00000000      -9.84375000       0.00000000       54.1406250      -0.00000000      -93.8437500       0.00000000       50.2734375
! m = 9
!    0.00000000       2.46093750      -0.00000000      -36.0937500       0.00000000       140.765625      -0.00000000      -201.093750       0.00000000       94.9609375
! m = 10
!  -0.246093750       0.00000000       13.5351562      -0.00000000      -117.304688       0.00000000       351.914062      -0.00000000      -427.324219       0.00000000       180.425781
