!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module solution_linear

    use precision_definitions, only : is, wp, zero, one
    use data, only : x, y, z, numDataPts

    implicit none
    character ( len = * ), parameter, private :: myModule = 'module solution_linear'  ! self-identification

    type :: intermediate_values
        real ( wp ) = det
        real ( wp ) = sumX, sumX2, sumY, sumXY
        real ( wp ) = sigma, sse
    end type :: intermediate_values

    type :: solution_linear
        real ( wp )            :: fit_results ( 1 : 2 )
        real ( wp )            :: fit_errors ( 1 : 2 )
        integer ( is )         :: status
        character ( len = 512) :: message
    contains
        private
        procedure, public :: normal_linear => normal_linear_sub
    end type solution_linear

    private :: normal_linear_sub

contains
!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                normal_linear

        subroutine normal_linear_sub ( me )

            class ( solution_linear ), target     :: me
            type ( data ) :: myData
            type ( intermediate_values )          :: intermediates
            type ( intermediate_values ), pointer :: iv
            type ( Linear_fit_results )           :: results
            type ( Linear_fit_results ), pointer  :: res

            character ( len = * ), parameter :: mySubroutine = 'subroutine normal_linear_sub'  ! self-identification
            character ( len = * ), parameter :: callChain   = 'Call chain: ', myModule, ', ', mySubroutine, '.'

                status = 0

                if ( numDataPts <= 2 ) then
                    status = -2
                    write ( message, 200 ) numDataPts
                    return
                end if

!               intermediate sums
                iv  => intermediates
                    iv % sumX  = sum ( myData % x )
                    iv % sumx2 = sum ( myData % x * myData % x )
                    iv % sumY  = sum ( myData % y )
                    iv % sumXY = sum ( myData % x * myData % y )

!                   determinant of the product matrix
                    iv % det   = myData % numDataPts * iv % sumX2 - ( iv % sumX )** 2
  
!                   don't divide by 0
                    if ( det == zero ) then
                        status = -1
                        write ( message, 100 ) callChain
                        return
                    end if

!                   dangerous determinant
                    if ( abs( iv % det ) <= epsilon ( one ) ) then
                        status = 1
                        write ( message, 110 ) iv % det, epsilon ( one ), callChain
                        return
                    end if

!                   suspicious determinant
                    if ( abs( iv % det ) <= 5 * epsilon ( one ) ) then
                        status = 2
                        write ( message, 120 ) iv % det, epsilon ( one ), callChain
                        return
                    end if

!                   compute the fit parameters
                    me % fit_results = [ iv % sumX2, iv % numDataPts ]  / iv % det
                    me % fit_results( 1 ) = ( iv % sumx2 * iv % sumY  - iv % sumX * iv % sumXY )
                    me % fit_results( 2 ) = ( myData % numDataPts * iv % sumXY - iv % sumX * iv % sumY )
                    me % fit_results = me % fit_results  / iv % det
!                   compute the fit error
                    me % sigma = [ iv % sumX2, myData % numDataPts ]  / iv % det

!                   compute the error
                    sse = zero
                    z = me % fit_results( 1 ) + me % fit_results( 2 ) * myData % x
                    s2 = r2 / ( numDataPts - 2 )
                iv => null ( )

!     diagonal of curvature matrix (A^T A)^ (-1)
      kappa( 1 ) = sumX2 / det
      kappa( 2 ) = dblen / det
  
!     compute the uncertainties in the fit parameters
      do k = 1, 2
        sigma( k ) = dsqrt( s2 * kappa( k ) )
      end do

            return

 100        format ( 'Determinant = 0: No solution. ', g0 )
 110        format ( 'Determinant <= machine epsilon: determinant = ', g0, ', machine epsilon = ', g0, '.' )
 120        format ( 'Suspiciously small determinant: determinant = ', g0, ', machine epsilon = ', g0, '.' )

 200        format ( 'Insufficient data for a linear regression. ', /,
                     'Number of data points = ', g0, '.', /,
                     '1 point: undetermined', /,
                     '2 points: exact solution, no regression.' )

        end subroutine normal_linear_sub

end module solution_linear
