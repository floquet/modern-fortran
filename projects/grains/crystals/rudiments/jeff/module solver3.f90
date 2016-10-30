! http://stackoverflow.com/questions/16486822/fortran-explicit-interface
module solver3

    use precision_definitions, only : is, wp, zero

    implicit none

!       rank 2
        real    ( wp ), allocatable      :: A ( : , : )
!       rank 1
        real    ( wp )                   :: X ( 1 : 1024 ) = zero
        real    ( wp )                   :: Y ( 1 : 1024 ) = zero

        real    ( wp )                   :: solution        ( 1 : 3 ) = zero
        real    ( wp )                   :: errors_solution ( 1 : 3 ) = zero

        real    ( wp ), allocatable      :: errors ( : )                     ! residual error vector
        real    ( wp ), allocatable      :: residual_errors ( : )            ! residual error vector

        integer ( is ), allocatable      :: ones ( : )
        integer ( is ), allocatable      :: J    ( : )

    contains

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine left_hand_side ( X, rowCensus, ones, J )

            use precision_definitions, only : is, wp, ascii

            implicit none

!           slot variables
            real    ( wp ), intent ( IN )               :: X ( : )             ! from atom locations
            integer ( is ), intent ( IN )               :: rowCensus ( : )     ! number of rows in data set

!           local variables
            integer ( is )                              :: nPoints = 0, nRows = 0, alloc_status = 0
            integer ( is )                              :: index = 0, k = 0, l = 0  ! dummy counters

            character ( kind = ascii, len = 255 )       :: alloc_msg = " "
!                                                                              MEASURE ARRAYS
!               numbers of points and number of rows of data
                nPoints = size ( X )
                nRows   = size ( rowCensus )
!                                                                              ALLOCATION

!               allocate array of ones
                allocate ( ones ( 1 : nPoints ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, '( /, "Error allocating memory for integer ( is ) array ones" )' )
                    write ( *, '(    "requested size is ", g0 )' ) nPoints
                    write ( *, '(    "stat = " )'   ) alloc_status
                    write ( *, '(    "errmsg = " )' ) alloc_msg
                    write ( *, '(    "Fatal error - ending run inside subroutine left_hand_side", / )' )
                    stop
                end if

!               allocate array J
                allocate ( J ( 1 : nPoints ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, '( /, "Error allocating memory for integer ( is ) array J" )' )
                    write ( *, '(    "requested size is ", g0 )' ) nPoints
                    write ( *, '(    "stat = " )'   ) alloc_status
                    write ( *, '(    "errmsg = " )' ) alloc_msg
                    write ( *, '(    "Fatal error - ending run inside subroutine left_hand_side", / )' )
                    stop
                end if
!                                                                              POPULATION
                ones = 1
!               number of points in each row e.g. J = [ [ 0, 0, 0 ], [ 1, 1 ], [ 2, 2, 2, 2 ], ... ]
                index = 1
                do l = 1, nRows
                    do k = 1, rowCensus ( l )
                      J ( index ) = l - 1
                      index = index + 1
                    end do  ! points within row
                end do      ! number of rows

        end subroutine left_hand_side

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine solve_linear_system ( ones, J, X, Y, solution, errors )

            use precision_definitions, only : is, wp, ascii, zero

            implicit none

!           slot variables
            real    ( wp ), intent ( IN )  :: X ( : ), Y ( : )                 ! from atom locations
            integer ( is ), intent ( IN )  :: ones ( : ), J ( : )              ! system matrix

            real    ( wp ), intent ( OUT ) :: solution ( 1 : 3 )               ! intercept, slope, row spacing
            real    ( wp ), intent ( OUT ) :: errors   ( 1 : 3 )               ! errors in intercept, slope, and row spacing

!           local variables

!           rank 2
            real    ( wp )                 :: Winv ( 1 : 3, 1 : 3 ) = zero     ! inverse of symmetric matrix W = A'A
!           rank 1
            real    ( wp )                 :: solution_errors ( 1 : 3 )        ! errors for solution vector
            real    ( wp )                 :: beta ( 1 : 3 ) = zero            ! solution vector
!           rank 0
            real    ( wp )                 :: det = zero                       ! determinant
            real    ( wp )                 :: SSE = zero                       ! sum of squared errors
            real    ( wp )                 :: a = zero, b = zero, c = zero, d = zero, e = zero, f = zero  ! unique elements

!           rank 0
            integer ( is )                 :: nPoints = 0, alloc_status = 0, k = 0

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

!               populate unique elements
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
                    write ( *, '( /, "Error allocating memory for real ( wp ) system matrix matrix_A" )' )
                    write ( *, '(    "requested size is ", g0, " x 3 " )' ) nPoints
                    write ( *, '(    "stat = " )'   ) alloc_status
                    write ( *, '(    "errmsg = " )' ) alloc_msg
                    write ( *, '(    "Fatal error - ending run inside subroutine solve_linear_system", / )' )
                    stop
                end if

!               allocate vector of residual errors
                allocate ( residual_errors ( 1 : nPoints ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, '( /, "Error allocating memory for real ( wp ) vector residual_errors" )' )
                    write ( *, '(    "requested size is ", g0 )' ) nPoints
                    write ( *, '(    "stat = " )'   ) alloc_status
                    write ( *, '(    "errmsg = " )' ) alloc_msg
                    write ( *, '(    "Fatal error - ending run inside subroutine solve_linear_system", / )' )
                    stop
                end if

                matrix_A ( : , 1 ) = ones
                matrix_A ( : , 2 ) = J
                matrix_A ( : , 3 ) = X

                residual_errors = matmul ( matrix_A, solution ) - Y
                SSE = dot_product ( residual_errors, residual_errors )         ! sum of squared errors

!               Data analysis and error analysis for the physical sciences, 1e
!               Philip Bevingtion, section 6.4 "Estimation of errors"
                solution_errors = [ ( Winv ( k, k ), k = 1, 3 ) ]              ! diagonal elements of Winv
!               scale by estimated parent standard deviation (SSE)
                solution_errors = solution_errors * SSE / ( nPoints - 3 )
                solution_errors = sqrt ( solution_errors )

        end subroutine solve_linear_system


!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

        subroutine solver ( ones, J, X, Y, solution )

            use precision_definitions, only : is, wp, zero

            implicit none

!           slot variables
            real    ( wp ), intent ( IN )  :: X ( : ), Y ( : )
            real    ( wp ), intent ( OUT ) :: solution ( 1 : 3 )
            integer ( is ), intent ( IN )  :: ones ( : ), J ( : )

!           rank 2
            real    ( wp )                 :: Winv ( 1 : 3, 1 : 3 ) = zero        ! symmetric matrix A'A
!           rank 1
            real    ( wp )                 :: beta ( 1 : 3 ) = zero               ! solution vector
!           rank 0
            real    ( wp )                 :: det = zero                          ! determinant
            real    ( wp )                 :: a = zero, b = zero, c = zero, d = zero, e = zero, f = zero  ! unique elements

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

!               populate unique elements
                Winv (   :  , 1 ) = [ d * f - e ** 2, c * e - b * f,  b * e - c * d ]
                Winv ( 2 : 3, 2 ) = [ a * f - c ** 2, b * c - a * e ]
                Winv ( 3 : 3, 3 ) = [ a * d - b ** 2 ]

!               populate repeated elements using symmetry
                Winv ( 1, 2 ) = Winv ( 2, 1 )
                Winv ( 1, 3 ) = Winv ( 3, 1 )
                Winv ( 2, 3 ) = Winv ( 3, 2 )

                Winv = Winv / det                                              ! inverse matrix
                solution = matmul ( Winv, beta )                               ! solution vector

                return

        end subroutine solver

!       + + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ + ++ +

    subroutine reader

        use precision_definitions, only : is, wp, zero
        use solver3

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
                write ( *, * ) 'io unit = ', io_unit
                write ( *, * ) 'iostat  = ', io_status
                write ( *, * ) 'iomsg   = ', io_msg
                stop 'Fatal error; execution will terminate'
           end if

!           READ DATA
            do k = 1, 1024                                                     ! bad idea: hard code file length
                read ( io_unit, *, iostat = io_status, iomsg = io_msg ) x ( k ), y ( k )
                if ( io_status /= 0 ) then                                         ! can't read data
                    write ( *, * )
                    write ( *, * ) 'READ ERROR: file ', fname
                    write ( *, * ) 'attempting to read line ', k
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
                write ( *, * ) 'io unit = ', io_unit
                write ( *, * ) 'iostat  = ', io_status
                write ( *, * ) 'iomsg   = ', io_msg
                write ( *, * ) 'Non-fatal error; execution will continue'
            end if

    end subroutine reader

end module solver3