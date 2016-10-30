! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 12 01

include 'sharedModules/mod precision definitions.f08'

program flintstone

    use mPrecisionDefinitions

    implicit none

    integer ( ip ), parameter :: n = 2

    type :: data
        real ( rp ), dimension ( 1 : 9 ) :: x = zero, y = zero, ones = one
        integer ( ip )                   :: m = 9
    end type data

    type :: intermediates
        real ( rp ) :: em = zero, sx = zero, sx2 = zero, sy = zero, sxy = zero
    end type intermediates

    type :: matrices
        real ( rp ), dimension ( 1 : n, 1 : n ) :: ASAinv = zero
        real ( rp ), dimension ( 1 : n )        :: B      = zero
        real ( rp )                             :: det    = zero
    end type matrices

    type :: results
        real ( rp ), dimension ( 1 : 2 ) :: a = 0
    end type results

    type ( data )          :: myData
    type ( intermediates ) :: myIntermediates
    type ( matrices )      :: myMatrices
    type ( results )       :: myResults

    character ( len = * ), parameter :: myProgram = 'program flintstone'  ! self-identification

!       load data
        myData % x ( 1 ) = 1.
        myData % x ( 2 ) = 2.
        myData % x ( 3 ) = 3.
        myData % x ( 4 ) = 4.
        myData % x ( 5 ) = 5.
        myData % x ( 6 ) = 6.
        myData % x ( 7 ) = 7.
        myData % x ( 8 ) = 8.
        myData % x ( 9 ) = 9.

        myData % y ( 1 ) = 15.6
        myData % y ( 2 ) = 17.5
        myData % y ( 3 ) = 36.6
        myData % y ( 4 ) = 43.8
        myData % y ( 5 ) = 58.2
        myData % y ( 6 ) = 61.6
        myData % y ( 7 ) = 64.2
        myData % y ( 8 ) = 70.4
        myData % y ( 9 ) = 98.8

!       compute intermediates
        myIntermediates % em  = dot_product ( myData % ones, myData % ones )
        myIntermediates % sx  = dot_product ( myData % ones, myData % x )
        myIntermediates % sy  = dot_product ( myData % ones, myData % y )
        myIntermediates % sx2 = dot_product ( myData % x,    myData % x )
        myIntermediates % sxy = dot_product ( myData % x,    myData % y )

!       compute matrices
        myMatrices % det = myData % m * myIntermediates % sx2 - myIntermediates % sx ** 2
        myMatrices % ASAinv ( : , 1 ) = [ myIntermediates % sx2, -myIntermediates % sx ]
        myMatrices % ASAinv ( : , 2 ) = [-myIntermediates % sx,   myIntermediates % em ]
        myMatrices % ASAinv = myMatrices % ASAinv / myMatrices % det
        myMatrices % B = [ myIntermediates % sy, myIntermediates % sxy ]

!       compute results
        myResults % a = matmul ( myMatrices % ASAinv, myMatrices % B )

        print *, "intercept = ", myResults % a ( 1 )
        print *, "slope     = ", myResults % a ( 2 )

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

end program flintstone