! http://stackoverflow.com/questions/16486822/fortran-explicit-interface
module linear_solver

    use precision_definitions, only : is, wp, ascii, zero

    implicit none

!   INPUTS:
!       X, Y: column vectors from the set of nPoints ordered pairs (x, y)
!       rowCensus: a census list describing how many points are in each row

!   OUTPUTS:
!       solution:         least squares parameters = [ intercept, row spacing, slope ]
!       errors:           errors associate with each solution parameter = [ \sigma_intercept, \sigma_{row spacing}, \sigma_slope ]
!       residual_errors:  A solution - Y

!   METHOD:
!       A solution = Y    linear system
!       W = TRANSPOSE ( A ) A    product matrix
!       solution = Winv TRANSPOSE ( A ) Y

!       output variables
        real    ( wp ), public                      :: solution        ( 1 : 3 ) ! intercept, row spacing, slope
        real    ( wp ), public                      :: errors_solution ( 1 : 3 ) ! errors in solution
        real    ( wp ), public, allocatable         :: errors_residual ( : )     ! residual error vector r = A alpha - y

!       atomic locations
        integer ( is ), private, parameter          :: numData = 1024            ! bad idea: hard code file length
        real    ( wp ), private                     :: x_all ( 1 : numData ), y_all ( 1 : numData )

!       intermediate variables
        real    ( wp ), private, allocatable        :: matrix_A ( : , : )        ! system matrix A

        integer ( is ), private, allocatable        :: ones ( : )                ! vector of 1 repeated m times
        integer ( is ), private, allocatable        :: J    ( : )                ! click counter
        integer ( is )                              :: alloc_status = 0

        character ( len = 511 )                     :: alloc_msg = " "
        character ( len =   * ), private, parameter :: name_mod  = "linear_solver"

    contains

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine least_squares_solution_pts ( pts, rowCensus )               ! wrapper routine: converts point indices to ( x, y )

            implicit none

!           slot variables
            integer ( is ), intent ( IN )               :: pts ( : )           ! list of points
            integer ( is ), intent ( IN )               :: rowCensus ( : )     ! number of rows in data set

            integer ( is )                              :: length              ! number of points in data set

!               count the data points
                length = size ( pts )

                call least_squares_solution ( x_all ( pts ), y_all ( pts ), rowCensus )

                return

        end subroutine least_squares_solution_pts

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine least_squares_solution ( X, Y, rowCensus )                  ! wrapper routine to create and solve linear system

            implicit none

!           slot variables
            real    ( wp ), intent ( IN )         :: X ( : ), Y ( : )          ! from atom locations
            integer ( is ), intent ( IN )         :: rowCensus ( : )           ! number of rows in data set

            character ( len = * ), parameter      :: name_sub = "least_squares_solution"

            call column_vectors_A ( X, rowCensus, name_sub )
            call solve_linear_system ( X, Y, name_sub )

        end subroutine least_squares_solution


!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine column_vectors_A ( X, rowCensus, name_call )                ! create A using column vectors

            implicit none

!           slot variables
            real    ( wp ), intent ( IN )         :: X ( : )                   ! from atom locations
            integer ( is ), intent ( IN )         :: rowCensus ( : )           ! number of rows in data set
            character ( len = * ), intent ( IN )  :: name_call

!           local variables
            integer ( is )                        :: nPoints = 0, nRows = 0, alloc_status = 0
            integer ( is )                        :: index = 0, k = 0, l = 0   ! dummy counters

            character ( len = * ), parameter      :: name_sub = "column_vectors_A"
            character ( kind = ascii, len = 255 ) :: alloc_msg = " "
!                                                                              MEASURE ARRAYS
!               numbers of points and number of rows of data
                nPoints = size ( X )
                nRows   = size ( rowCensus )
!                                                                              ALLOCATION

!               allocate array of ones
                allocate ( ones ( 1 : nPoints ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "integer ( is ) array ones"
                    write ( *, 110 ) nPoints
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 )
                    write ( *, 150 ) name_sub
                    write ( *, 160 ) name_call
                    write ( *, 170 ) name_mod
                    stop
                end if

!               allocate array J
                allocate ( J ( 1 : nPoints ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "integer ( is ) array J"
                    write ( *, 110 ) nPoints
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 )
                    write ( *, 150 ) name_sub
                    write ( *, 160 ) name_call
                    write ( *, 170 ) name_mod
                    stop
                end if
!                                                                              POPULATION
                ones ( : ) = 1
!               number of points in each row e.g. J = [ [ 0, 0, 0 ], [ 1, 1 ], [ 2, 2, 2, 2 ], ... ]
                index = 1
                do l = 1, nRows
                    do k = 1, rowCensus ( l )
                      J ( index ) = l - 1
                      index = index + 1
                    end do  ! points within row
                end do      ! number of rows

!           next we construct A = [ 1 J X ]
            return

  100       format ( /, "Error allocating memory for ", A )
  110       format (    "requested size is ", g0 )
  120       format (    "stat = ", g0 )
  130       format (    "errmsg = ", A, "." )
  140       format (    "Fatal error - ending run" )
  150       format (    "inside subroutine ", A )
  160       format (    "called from subroutine ", A )
  170       format (    "inside module ", A )

        end subroutine column_vectors_A

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine solve_linear_system ( X, Y, name_call )                     ! solve the normal equations by constructing
                                                                               ! the inverse of W = A' A

            implicit none

!           slot variables
            real    ( wp ), intent ( IN )         :: X ( : ), Y ( : )          ! from atom locations
            character ( len = * ), intent ( IN )  :: name_call

!           local variables

!           rank 2
            real    ( wp )                        :: Winv ( 1 : 3, 1 : 3 ) = zero ! inverse of symmetric matrix W = A'A
!           rank 1
            real    ( wp )                        :: beta ( 1 : 3 ) = zero     ! solution vector
!           rank 0
            real    ( wp )                        :: det = zero                ! determinant
            real    ( wp )                        :: SSE = zero                ! sum of squared errors
            real    ( wp )                        :: a = zero, b = zero, c = zero, d = zero, e = zero, f = zero  ! unique elements

!           rank 0
            integer ( is )                        :: nPoints = 0, alloc_status = 0, k = 0

            character ( len = * ), parameter      :: name_sub = "solve_linear_system"
            character ( kind = ascii, len = 255 ) :: alloc_msg = " "

!                                                                              ! SOLUTION
!               unique elements in product matrix A'A
                a = dot_product ( ones, ones )
                b = dot_product ( ones, J )
                c = dot_product ( ones, X )
                d = dot_product ( J, J )
                e = dot_product ( J, X )
                f = dot_product ( X, X )

!               determinant
                det = 2 * b * c * e + a * d * f - a * e ** 2 - d * c ** 2 - f * b ** 2

!               data vector
                beta ( 1 ) = dot_product ( ones, Y )
                beta ( 2 ) = dot_product ( J,    Y )
                beta ( 3 ) = dot_product ( X,    Y )

!               populate unique elements in inverse matrix
                Winv (   :  , 1 ) = [ d * f - e ** 2, c * e - b * f,  b * e - c * d ]
                Winv ( 2 : 3, 2 ) = [ a * f - c ** 2, b * c - a * e ]
                Winv ( 3 : 3, 3 ) = [ a * d - b ** 2 ]

!               populate repeated elements using symmetry
                Winv ( 1, 2 ) = Winv ( 2, 1 )
                Winv ( 1, 3 ) = Winv ( 3, 1 )
                Winv ( 2, 3 ) = Winv ( 3, 2 )

                Winv = Winv / det                                              ! inverse matrix

                solution = matmul ( Winv, beta )                               ! solution vector

!                                                                              ! ERRORS
!               numbers of points and number of rows of data
                nPoints = size ( X )

!               allocate system matrix A
                allocate ( matrix_A ( 1 : nPoints, 1 : 3 ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "real ( wp ) system matrix matrix_A"
                    write ( *, 110 ) nPoints
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 )
                    write ( *, 150 ) name_sub
                    write ( *, 160 ) name_call
                    write ( *, 170 ) name_mod
                    stop
                end if

!               allocate vector of residual errors
                allocate ( errors_residual ( 1 : nPoints ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "real ( wp ) vector residual_errors"
                    write ( *, 110 ) nPoints
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 )
                    write ( *, 150 ) name_sub
                    write ( *, 160 ) name_call
                    write ( *, 170 ) name_mod
                    stop
                end if

!               construct system matrix A using column vectors
                matrix_A ( : , 1 ) = ones
                matrix_A ( : , 2 ) = J
                matrix_A ( : , 3 ) = X

                errors_residual = matmul ( matrix_A, solution ) - Y

!               Data analysis and error analysis for the physical sciences, 1e
!               Philip Bevingtion, section 6.4 "Estimation of errors"
                SSE = dot_product ( errors_residual, errors_residual )         ! sum of squared errors
                errors_solution = [ ( Winv ( k, k ), k = 1, 3 ) ]              ! diagonal elements of Winv
!               scale by estimated parent standard deviation (SSE)
                errors_solution = sqrt ( errors_solution * SSE / ( nPoints - 3 ) )

!               deallocate system matrix A
                deallocate ( matrix_A, stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 200 ) "real ( wp ) system matrix matrix_A"
                    write ( *, 210 ) size ( matrix_A )
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 )
                    write ( *, 150 ) name_sub
                    write ( *, 160 ) name_call
                    write ( *, 170 ) name_mod
                    stop
                end if

!               deallocate vector of residual errors
                deallocate ( errors_residual, stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 200 ) "real ( wp ) vector residual_errors"
                    write ( *, 210 ) size ( errors_residual )
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 )
                    write ( *, 150 ) name_sub
                    write ( *, 160 ) name_call
                    write ( *, 170 ) name_mod
                    stop
                end if

!               deallocate vector of ones
                deallocate ( ones, stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 200 ) "real ( wp ) vector ones"
                    write ( *, 210 ) size ( ones )
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 )
                    write ( *, 150 ) name_sub
                    write ( *, 160 ) name_call
                    write ( *, 170 ) name_mod
                    stop
                end if

!               deallocate vector of clicks
                deallocate ( J, stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 200 ) "integer ( is ) vector J"
                    write ( *, 210 ) size ( J )
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 )
                    write ( *, 150 ) name_sub
                    write ( *, 160 ) name_call
                    write ( *, 170 ) name_mod
                    stop
                end if
            return

  100       format ( /, "Error allocating memory for ", A )
  110       format (    "requested size is ", g0, " x 3" )
  120       format (    "stat = ", g0 )
  130       format (    "errmsg = ", A, "." )
  140       format (    "Fatal error - ending run" )
  150       format (    "inside subroutine ", A )
  160       format (    "called from subroutine ", A )
  170       format (    "inside module ", A )

  200       format ( /, "Error deallocating memory for ", A )
  210       format (    "current size is ", g0 )

        end subroutine solve_linear_system

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

    subroutine reader ( )   !  reads atomic locations from simulation file

!         Raw data comes from the simulation
!         Columns are in format x y 1
!         File has 1024 lines:

!         -0.158790010738178E+02 -0.163654959719242E+02  0.100000000000000E+01
!         -0.147494455669407E+02 -0.159954884350967E+02  0.100000000000000E+01
!                   .                       .                      .

        implicit NONE

        integer ( is )                     :: io_unit = 0, io_status = 0
        integer ( is )                     :: k = 0

        character ( len =  11 ), parameter :: fname  = 'xy_data.txt'           ! data from Jeff's simulation
        character ( len =   * ), parameter :: me     = 'subroutine reader'     ! Metcalf, Reid, Cohen: p. 309
        character ( len = 511 )            :: io_msg = " "

!           OPEN FILE
            open ( file = fname, newunit = io_unit, status = 'OLD', iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then                                         ! can't open file
                write ( *, * )
                write ( *, * ) 'ERROR: unable to open file ', fname
                write ( *, * ) 'look in ', me
                write ( *, * ) 'io unit = ', io_unit
                write ( *, * ) 'iostat  = ', io_status
                write ( *, * ) 'iomsg   = ', io_msg
                stop 'Fatal error; execution will terminate'
           end if

!           READ DATA
            do k = 1, numData                                                  ! bad idea: hard code file length
                read ( io_unit, *, iostat = io_status, iomsg = io_msg ) x_all ( k ), y_all ( k )
                if ( io_status /= 0 ) then                                     ! can't read data
                    write ( *, * )
                    write ( *, * ) 'READ ERROR: file ', fname
                    write ( *, * ) 'attempting to read line ', k
                    write ( *, * ) 'look in ', me
                    write ( *, * ) 'io unit = ', io_unit
                    write ( *, * ) 'iostat  = ', io_status
                    write ( *, * ) 'iomsg   = ', io_msg
                    write ( *, * ) 'Non-fatal error; execution will continue'
                end if
            end do

!           CLOSE FILE
            close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
            if ( io_status /= 0 ) then                                         ! can't close file
                write ( *, * )
                write ( *, * ) 'ERROR: unable to close file ', fname
                write ( *, * ) 'look in ', me
                write ( *, * ) 'io unit = ', io_unit
                write ( *, * ) 'iostat  = ', io_status
                write ( *, * ) 'iomsg   = ', io_msg
                write ( *, * ) 'Non-fatal error; execution will continue'
            end if

    end subroutine reader

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

    subroutine grade_card ( soln_fortran, soln_mathematica, error_fortran, error_mathematica, pass, axis )

        implicit NONE

        real      ( wp ),              intent ( IN )    :: soln_fortran     ( : ), error_fortran     ( : )
        real      ( wp ),              intent ( IN )    :: soln_mathematica ( : ), error_mathematica ( : )

        character ( len = * ),         intent ( IN )    :: axis
        logical   ( kind ( .true. ) ), intent ( INOUT ) :: pass

        real      ( wp )                                :: value = zero, diff = zero, err = zero
        real      ( wp )                                :: good = 1.0e13_wp

!           grade card
            write ( *, '( /, "comparison with Mathematica for ", g0,"..." )' ) axis
            write ( *, '(    "solution parameters..." )' )
            value = soln_fortran ( 1 )
            diff  = value - soln_mathematica ( 1 )
            err   = abs ( diff / value )
            if ( err > good ) pass = pass .and. .false.
            write ( *, 200 ) "offset,  alpha 0", diff, err, value

            value = soln_fortran ( 2 )
            diff  = value - soln_mathematica ( 2 )
            err   = diff / value
            err   = abs ( diff / value )
            if ( err > good ) pass = pass .and. .false.
            write ( *, 200 ) "spacing, alpha *", diff, err, value

            value = soln_fortran ( 3 )
            diff  = value - soln_mathematica ( 3 )
            err   = abs ( diff / value )
            if ( err > good ) pass = pass .and. .false.
            write ( *, 200 ) "slope,   alpha 1", diff, err, value

            write ( *, '( /, "errors in solution parameters..." )' )
            value = error_fortran ( 1 )
            diff  = value - error_mathematica ( 1 )
            err   = abs ( diff / value )
            if ( err > good ) pass = pass .and. .false.
            write ( *, 200 ) "error in offset,  alpha 0", diff, err, value

            value = error_fortran ( 2 )
            diff  = value - error_mathematica ( 2 )
            err   = abs ( diff / value )
            if ( err > good ) pass = pass .and. .false.
            write ( *, 200 ) "error in spacing, alpha *", diff, err, value

            value = error_fortran ( 3 )
            diff  = value - error_mathematica ( 3 )
            err   = abs ( diff / value )
            if ( err > good ) pass = pass .and. .false.
            write ( *, 200 ) "error in slope,   alpha 1", diff, err, value

            return

  200       format ( A, ": difference = ", g0, ", (relative error = ", E10.2, ") [ value = ", g0, "]" )

    end subroutine grade_card

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

end module linear_solver