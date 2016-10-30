!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module solution_normal

    use precision_definitions, only : is, wp, zero, one
    use data, only : x, y, numDataPts

    implicit none
    character ( len = * ), parameter, private :: myModule = 'module solution_normal'  ! self-identification

    type :: intermediate_values
        real ( wp ) = det, sumX, sumX2, sumY, sumXY
    end type :: intermediate_values

    type :: solution_linear
        real ( wp ) = intercept_value
        real ( wp ) = slope_value
        real ( wp ) = intercept_error
        real ( wp ) = slope_error
        integer ( is )         :: status
        character ( len = 512) :: message
    contains
        private
        public, public :: normal_linear => normal_linear_sub
    end type solution_linear

    private :: normal_linear_sub

contains
!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                normal_linear

        subroutine normal_linear_sub ( )

            class ( solution_linear ), target     :: me
            type ( intermediate_values )          :: intermediates
            type ( intermediate_values ), pointer :: iv

            character ( len = * ), parameter :: mySubroutine = 'subroutine normal_linear_sub'  ! self-identification

                status = 0
!               intermediate sums
                iv  => intermediates
                    iv % sumX  = sum ( x )
                    iv % sumx2 = sum ( x * x )
                    iv % sumY  = sum ( y )
                    iv % sumXY = sum ( x * y)

!                   determinant of the product matrix
                    iv % det   = numDataPts * iv % sumX2 - iv % sumX * iv % sumX
  
!                   don't divide by 0
                    if ( det == zero ) then
                        status = -1
                        write ( message, 100 ) myModule, mySubroutine
                        return
                    end if

                    if ( abs( iv % det ) <= epsilon ( one ) ) then
                        status = 1
                        write ( message, 110 ) iv % det
                        message = "Determinant = 0: No solution. Call chain: " // myModule // ", " // mySubroutine // "."
                    end if
            iv => null ( )

!           compute the fit parameters
            sigma = [ sumX2 / det, dblen / det ]
            soln( 1 ) = ( sumx2 * sumY  - sumX * sumXY ) / det
            soln( 2 ) = ( dblen * sumXY - sumX * sumY )  / det

!     compute the error      
      r2 = 0.D0
      do k = 1, n
        predict = soln( 2 ) * data( k, 1 ) + soln( 1 )
        r2      = r2 + ( data( k, 2 ) - predict )**2  ! prediction - measurement
      end do
      s2 = r2 / dble( n - 2 )
  
!     diagonal of curvature matrix (A^T A)^ (-1)
      kappa( 1 ) = sumX2 / det
      kappa( 2 ) = dblen / det
  
!     compute the uncertainties in the fit parameters
      do k = 1, 2
        sigma( k ) = dsqrt( s2 * kappa( k ) )
      end do

            return

 100        format ( 'Determinant = 0: No solution. Call chain: ', g0, ', ', g0, '.' )
 110        format ( 'Suspiciously small determinant = ', g0 )

        end subroutine normal_linear_sub

end module normal_linear_sub
