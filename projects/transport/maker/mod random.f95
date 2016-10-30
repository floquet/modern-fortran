module mRandom

    use iso_fortran_env
    use precision_definitions, only : wp, pi, is, zero, one
    use param_simulation
    implicit none

    real ( wp ), allocatable         :: randomList        ( : , : )
    real ( wp ), allocatable         :: randomPolarList   ( : , : )
    real ( wp ), allocatable         :: randomAnnularList ( : , : )

    integer ( is ), private          :: kMod

    integer ( is ), private          :: alloc_status
    character ( len = 512 ), private :: alloc_msg

    character ( len = * ), private, parameter :: me_module_random = 'module mRandom'  ! self-identification

    contains

!       ############################################################################################
!       #                                                                                          #
!       #  Lists                                                                                   #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       random_polar_distribution

        subroutine random_polar_distribution_sub ( numPoints, r_inner, r_outer )

            integer ( is ), intent ( in ) :: numPoints
            real ( wp ), intent ( in )    :: r_inner, r_outer

            real ( wp ) :: radius, theta, scalar

            character ( len = 512 ), parameter :: me_subroutine = 'subroutine random_polar_distribution_sub'  ! self-identification
            character ( len = * ),   parameter :: stop_msg = 'Halting on execution error in ' // me_module_random // ', ' &
                                                                                            // me_subroutine // '.'

!               list of ( r, theta ) points
                if ( allocated ( randomPolarList ) ) then
                    deallocate ( randomPolarList, stat = alloc_status, errmsg = alloc_msg )
                    if ( alloc_status /= 0 ) then
                        write ( *, 100 ) "de", "real ( wp )", "randomPolarList"
                        write ( *, 110 ) size ( randomPolarList ), sizeof ( randomPolarList )
                        write ( *, 120 ) alloc_status
                        write ( *, 130 ) trim ( alloc_msg )
                        stop stop_msg
                    end if
                end if

                allocate ( randomPolarList ( 1 : numPoints, 1 : numDims ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "", "real ( wp )", "randomPolarList"
                    write ( *, 140 ) numPoints, numDims
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    stop stop_msg
                end if

                do kMod = 1, numPoints
                    call random_number ( scalar )
                    radius = r_inner + r_outer * scalar
                    call random_number ( scalar )
                    theta = 2 * pi * scalar
                    randomPolarList ( kMod, : ) = radius * [ cos ( theta ), sin ( theta ) ]
                    write ( * , 200 ) kMod, radius * [ cos ( theta ), sin ( theta ) ]
                end do

                return

    100         format ( /, "Error ", g0, "allocating memory for ", g0, " array ", g0, "." )
    110         format (    "  current size is ", 10I15, " elements ( ", g0," bytes)" )
    120         format (    "  stat = ", g0 )
    130         format (    "  errmsg = ", g0, "." )
    140         format (    "  requested size is ", g0, " x ", g0," elements" )

    200         format ( I9, '. ', 2( F10.3 ) )

        end subroutine random_polar_distribution_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     random_distribution_annulus

        subroutine random_distribution_annulus_sub ( numPoints, r_inner, r_outer )

            integer ( is ), intent ( in ) :: numPoints
            real ( wp ), intent ( in )    :: r_inner, r_outer

            character ( len = 512 ), parameter :: me_subroutine = 'subroutine random_distribution_annulus_sub'  !self-identification
            character ( len = * ),   parameter :: stop_msg = 'Halting on execution error in ' // me_module_random // ', ' &
                                                                                              // me_subroutine // '.'

!               allocate list of points
                if ( allocated ( randomAnnularList ) ) then
                    deallocate ( randomAnnularList, stat = alloc_status, errmsg = alloc_msg )
                    if ( alloc_status /= 0 ) then
                        write ( *, 100 ) "de", "real ( wp )", "randomAnnularList"
                        write ( *, 110 ) size ( randomAnnularList ), sizeof ( randomAnnularList )
                        write ( *, 120 ) alloc_status
                        write ( *, 130 ) trim ( alloc_msg )
                        stop stop_msg
                    end if
                end if

                allocate ( randomAnnularList ( 1 : numPoints, 1 : numDims ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "", "real ( wp )", "randomAnnularList"
                    write ( *, 140 ) numPoints, numDims
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    stop stop_msg
                end if

!               assign those points within domain
                !do concurrent ( kMod = 1 : numPoints )
                do kMod = 1, numPoints
                    randomAnnularList ( kMod, : ) = random_zeta_point_fcn ( r_inner, r_outer )
                end do

                return

    100         format ( /, "Error ", g0, "allocating memory for ", g0, " array ", g0, "." )
    110         format (    "  current size is ", 10I15, " elements ( ", g0," bytes)" )
    120         format (    "  stat = ", g0 )
    130         format (    "  errmsg = ", g0, "." )
    140         format (    "  requested size is ", g0, " x ", g0," elements" )

        end subroutine random_distribution_annulus_sub

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                    random_vector_of_points

        subroutine random_vector_of_points_sub ( numPoints )

            integer ( is ), intent ( in ) :: numPoints

            real ( wp ) :: point ( 1 : numDims )

            character ( len = 512 ), parameter :: me_subroutine = 'subroutine random_vector_of_points_sub'  ! self-identification
            character ( len = * ),   parameter :: stop_msg = 'Halting on execution error in ' // me_module_random // ', ' &
                                                                                              // me_subroutine // '.'

!               list of points
                if ( allocated ( randomList ) ) then
                    deallocate ( randomList, stat = alloc_status, errmsg = alloc_msg )
                    if ( alloc_status /= 0 ) then
                        write ( *, 100 ) "de", "real ( wp )", "randomPolarList"
                        write ( *, 110 ) size ( randomList ), sizeof ( randomList )
                        write ( *, 120 ) alloc_status
                        write ( *, 130 ) trim ( alloc_msg )
                        stop stop_msg
                    end if
                end if

                allocate ( randomList ( 1 : numPoints, 1 : numDims ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "", "real ( wp )", "numPoints"
                    write ( *, 140 ) numPoints, numDims
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    stop stop_msg
                end if

                do kMod = 1, numPoints
                    call random_number ( point )
                    randomList ( kMod, : ) = point
                end do

                return

    100         format ( /, "Error ", g0, "allocating memory for ", g0, " array ", g0, "." )
    110         format (    "  current size is ", 10I15, " elements ( ", g0," bytes)" )
    120         format (    "  stat = ", g0 )
    130         format (    "  errmsg = ", g0, "." )
    140         format (    "  requested size is ", g0, " x ", g0," elements" )

        end subroutine random_vector_of_points_sub

!       ############################################################################################
!       #                                                                                          #
!       #  Vectors                                                                                 #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                random_direction

        function random_direction_fcn ( ) result ( unit_vector )

            real ( wp ) :: theta
            real ( wp ) :: unit_vector ( 1 : numDims )

                theta = pi * random_zeta_fcn ( )

                unit_vector = [ Cos ( theta ), Sin ( theta ) ]

        end function random_direction_fcn

!       ############################################################################################
!       #                                                                                          #
!       #  Matrices                                                                                #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                       unit_ball

        function unit_ball ( nDim, nRotations ) result ( rotation_nDim )

            integer ( is ), intent ( in )    :: nDim, nRotations

            real ( wp )                      :: theta, c, s, x
            real ( wp ), allocatable         :: rotation_nDim ( : , : ), unit_nDim ( : , : ), givens_nDim ( : , : )

            integer ( is )                   :: alloc_status, dim1, dim2, j

            character ( len = 512 )          :: alloc_msg = "null"
            character ( len = * ), parameter :: me_subroutine = 'function unit_ball'  ! self-identification
            character ( len = * ), parameter :: stop_msg = 'Halting on execution error in ' // me_module_random // ', ' &
                                                                                            // me_subroutine // '.'

!               persistent identity matrix
                allocate ( unit_nDim ( 1 : nDim, 1 : nDim ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "", "real ( wp )", "unit_nDim"
                    write ( *, 110 ) nDim
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    stop stop_msg
                end if

!               Givens rotation matrix
                allocate ( givens_nDim ( 1 : nDim, 1 : nDim ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "", "real ( wp )", "givens_nDim"
                    write ( *, 110 ) nDim
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    stop stop_msg
                end if

!               result matrix
                allocate ( rotation_nDim ( 1 : nDim, 1 : nDim ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "", "real ( wp )", "rotation_nDim"
                    write ( *, 110 ) nDim
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    stop stop_msg
                end if

                ! create identity matrix
                unit_nDim ( : , : ) = zero
                do concurrent ( kMod = 1 : nDim )
                    unit_nDim ( kMod, kMod ) = one
                end do

                rotation_nDim ( : , : ) = unit_nDim ( : , : )
                do j = 1, nRotations  ! rotations in a random plane
                    givens_nDim   ( : , : ) = unit_nDim ( : , : )
                    do  ! random rotation plane
                        call random_number ( x )
                        dim1 = ceiling ( x * nDim )
                        call random_number ( x )
                        dim2 = ceiling ( x * nDim )
                        if ( dim1 /= dim2 ) exit
                    end do

                    theta = pi * random_zeta_fcn ( )  ! random angle
                    c = Cos ( theta )
                    s = Sin ( theta )

                    givens_nDim ( dim1, dim1 ) =  c  ! rotation matrix
                    givens_nDim ( dim2, dim2 ) =  c
                    givens_nDim ( dim1, dim2 ) =  s
                    givens_nDim ( dim2, dim1 ) = -s

                    rotation_nDim = matmul ( givens_nDim, rotation_nDim )  ! result
                end do

!               persistent identity matrix
                deallocate ( unit_nDim , stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "de", "real ( wp )", "unit_nDim"
                    write ( *, 110 ) nDim
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 ) me_module_random, me_subroutine
                end if

!               Givens rotation matrix
                deallocate ( givens_nDim, stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "de", "real ( wp )", "givens_nDim"
                    write ( *, 110 ) nDim
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    write ( *, 140 ) me_module_random, me_subroutine
                end if

                return

    100         format ( /, "Error ", g0, "allocating memory for ", g0, " array ", g0, "." )
    110         format (    "  requested size is ", g0, " elements" )
    120         format (    "  stat = ", g0 )
    130         format (    "  errmsg = ", g0, "." )
    140         format ( "Execution continues. Error encountered in ", g0, ", ", g0, "." )

        end function unit_ball

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      random_su2

        function random_su2_fcn ( ) result ( su_2 )

            real ( wp )                      :: theta, c, s
            real ( wp ), dimension ( 1 : 2 ) :: v1, v2 ! column vectors
            real ( wp )                      :: su_2 ( 1 : 2, 1 : 2 )

                theta = pi * random_zeta_fcn ( )

                c = Cos ( theta )
                s = Sin ( theta )

                v1 ( : ) = [  c, s ]  ! column vectors
                v2 ( : ) = [ -s, c ]

                su_2 = reshape ( [ v1, v2 ], [ 2, 2 ] )

        end function random_su2_fcn

!       ############################################################################################
!       #                                                                                          #
!       #  -1 <= x < 1                                                                             #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++               random_zeta_point

        function random_zeta_point_fcn ( r_inner, r_outer ) result ( zeta )  ! random point in annulus

            real ( wp ), intent ( in ) :: r_inner, r_outer
            real ( wp )                :: x ( 1 : numDims ), zeta ( 1 : numDims )
            real ( wp )                :: nrm

                do
!                   create random vector
                    call random_number ( x )  !  0 <= x < 1
                    zeta = 2 * x - one        ! -1 <= x < 1
                    zeta = zeta * r_outer
!                   accept only if inside domain
                    nrm = norm2 ( zeta )
                    if ( nrm < r_inner ) cycle
                    if ( nrm > r_outer ) cycle
                    return
                end do

        end function random_zeta_point_fcn

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     random_zeta

        function random_zeta_fcn ( ) result ( zeta )

            real ( wp ) :: x, zeta

                call random_number ( x )  !  0 <= x < 1
                zeta = 2 * x - one        ! -1 <= x < 1

        end function random_zeta_fcn

!       ############################################################################################
!       #                                                                                          #
!       #  Initialize RNG                                                                          #
!       #                                                                                          #
!       ############################################################################################

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                    random_point
!       https://stackoverflow.com/questions/3828094/function-returning-an-array-in-fortran

        function random_point_fcn ( ) result ( point )

            use param_simulation

            real ( wp ) :: point ( 1 : numDims )

                call random_number ( point )

        end function random_point_fcn

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                             lcg

        function lcg ( s )

            integer :: lcg
            integer ( int64 ), intent ( out ) :: s

                if ( s == 0 ) then
                    s = 104729
                else
                    s = mod ( s, 4294967296_int64 )
                end if
                s   = mod ( s * 279470273_int64, 4294967291_int64 )
                lcg = int ( mod ( s, int ( huge ( 0 ), int64 ) ), kind ( 0 ) )

        end function lcg

!       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                init_random_seed
!       https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html

        subroutine init_random_seed_sub ( checkOS, mySeed )

            integer, intent ( IN ), optional :: mySeed ( : )
            logical, intent ( IN ), optional :: checkOS

            integer, allocatable             :: seed ( : )
            integer                          :: alloc_status = 0
            integer                          :: k = 0, n = 0, un = 0, istat = 0, pid = 0, bytes = 0
            integer                          :: dt ( 8 ) = 0
            integer ( int64 )                :: t = 0, u = 0

            character ( len = 256 )          :: alloc_msg = ""
            character ( len = * ), parameter :: me_subroutine = 'subroutine init_random_seed_sub'  ! self-identification
            character ( len = * ), parameter :: stop_msg = 'Halting on execution error in ' // me_module_random // ', ' &
                                                                                            // me_subroutine // '.'

!               check for user-supplied seed
                if ( present ( mySeed ) ) then
                    call random_seed ( put = mySeed )
                    return
                end if  ! present ( mySeed )

!               allocate memory for seed
                call random_seed ( size = n )  ! measure seed size for allocation
                allocate ( seed ( n ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 100 ) "integer", "seed"
                    write ( *, 110 ) n
                    write ( *, 120 ) alloc_status
                    write ( *, 130 ) trim ( alloc_msg )
                    stop stop_msg
                end if

!               attempt to get seed from OS
                present_checkOS: if ( present ( checkOS ) ) then
                    if ( checkOS ) then
                        open ( newunit = un, file = "/dev/urandom", access = "stream", form = "unformatted", action = "read", &
                                                                    status = "old",  iostat = istat )
                        if ( istat == 0 ) then
                            read  ( un ) seed
                            close ( un )
                            call random_seed ( put = seed )
                            return
                        else
                            exit present_checkOS
                        end if ! istat == 0

                    end if  ! checkOS
                end if present_checkOS ! present ( checkOS )

!               Fallback to XOR:ing the current time and pid. 
!               The PID is useful in case one launches multiple instances of the same program in parallel.
                call system_clock ( t )

                if ( t == 0 ) then
                    call date_and_time ( values = dt )  ! convert time to milliseconds from t0
                    t = ( dt ( 1 ) - 1970 ) * 365_int64 * 24 * 60 * 60 * 1000 &  ! year
                        + dt ( 2 ) * 30_int64 * 24 * 60 * 60 * 1000           &  ! month
                        + dt ( 3 ) * 24_int64 * 60 * 60 * 1000                &  ! day
                        + dt ( 5 ) * 60 * 60 * 1000                           &  ! hour
                        + dt ( 6 ) * 60 * 1000                                &  ! minutes
                        + dt ( 7 ) * 1000                                     &  ! seconds
                        + dt ( 8 )                                               ! milliseconds
                end if  ! t == 0

                bytes = bit_size ( t ) / 8
                do k = 0, bytes - 1  ! move LSB in t to MSB in u
                    call mvbits( t, k * 8, 8, u, ( bytes - k - 1 ) * 8 )
                end do

                pid = getpid ( )  ! get process id
                u = ieor ( u, int ( pid, kind ( u ) ) )
                do k = 1, n
                    seed ( k ) = lcg ( u )
                end do

                call random_seed ( put = seed )

                return

    100         format ( /, "Error allocating memory for ", g0, " array ", g0, "." )
    110         format (    "  requested size is ", g0, " elements" )
    120         format (    "  stat = ", g0 )
    130         format (    "  errmsg = ", g0, "." )

        end subroutine init_random_seed_sub


end module mRandom