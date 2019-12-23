module mAllocator

    use mPrecisionDefinitions,  only : ip, rp
    use mParameters,            only : zero

    implicit none

    contains

        subroutine allocator ( A, B, F, flat_1, flat_2, m, n, p, census_1, census_2, census_3 )

            integer ( ip ), intent ( in )            :: m, n, p
            integer ( ip ), intent ( out )           :: census_1, census_2, census_3
            integer ( ip )                           :: alloc_status

            real ( rp ), intent ( out ), allocatable :: A ( : , : )
            real ( rp ), intent ( out ), allocatable :: B ( : , : )
            real ( rp ), intent ( out ), allocatable :: F ( : , : )

            real ( rp ), intent ( out ), allocatable :: flat_1 ( : )
            real ( rp ), intent ( out ), allocatable :: flat_2 ( : )

            real ( rp )                              :: t0 = zero, t1 = zero, delta_t = zero

            character ( len = 255 )                  :: alloc_msg = " "

                call cpu_time ( t0 )

                write ( *, '( "Beginning allocations ..." )' )

                census_1 = m * n
                census_2 = n * p
                census_3 = m * p

                write ( *, 100 ) "A", m, n, census_1
                allocate ( A ( 1 : m, 1 : n ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 120 )
                    write ( *, 130 ) alloc_status
                    write ( *, 140 ) alloc_msg
                end if

                write ( *, 100 ) "B", n, p, census_2
                allocate ( B ( 1 : n, 1 : p ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 120 )
                    write ( *, 130 ) alloc_status
                    write ( *, 140 ) alloc_msg
                end if

                write ( *, 100 ) "C", m, p, census_3
                allocate ( F ( 1 : m, 1 : p ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 120 )
                    write ( *, 130 ) alloc_status
                    write ( *, 140 ) alloc_msg
                end if

                write ( *, 110 ) "flat_1", census_1
                allocate ( flat_1 ( 1 : census_1 ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 120 )
                    write ( *, 130 ) alloc_status
                    write ( *, 140 ) alloc_msg
                end if

                write ( *, 110 ) "flat_2", census_2
                allocate ( flat_2 ( 1 : census_2 ), stat = alloc_status, errmsg = alloc_msg )
                if ( alloc_status /= 0 ) then
                    write ( *, 120 )
                    write ( *, 130 ) alloc_status
                    write ( *, 140 ) alloc_msg
                end if

                call cpu_time ( t1 )
                write ( *, 150 ) delta_t

                return

         100    format ( 'allocating rank two array ', A, ' with m = ', g0, ' with n = ', g0, ' for ', g0, ' total elements' )
         110    format ( 'allocating rank one array ', A, ' with ', g0, ' total elements' )
         120    format ( "failure to allocate real array" )
         130    format ( "allocation status variable stat = ", g0, "." )
         140    format ( "error message errmsg = ", A, "." )
         150    format ( E8.3, " seconds for all allocations", / )

        end subroutine allocator

end module mAllocator
