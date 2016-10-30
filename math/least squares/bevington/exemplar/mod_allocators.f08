module mAllocations

    use mPrecisionDefinitions,  only : ip, rp

    integer ( ip )          :: alloc_status
    character ( len = 512 ) :: alloc_message

    character ( len = * ), parameter :: fmt_allocproblem = '( "Error ", g0, " memory." )'
    character ( len = * ), parameter :: fmt_allocstat    = '( "stat = ", g0 )'
    character ( len = * ), parameter :: fmt_allocmsg     = '( "errmsg  = ", g0, "." )'

    contains

        !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +                 allocator_rank_2

        module subroutine allocator_rank_2_sub ( array, rows, cols )

            real ( rp ), allocatable, intent ( out ) :: array ( : , : )
            integer ( ip ),           intent ( in )  :: rows, cols

                ! deallocate array
                if ( allocated ( array ) ) then
                    deallocate ( array, stat = alloc_status, errmsg = alloc_message )
                    if ( alloc_status /= 0 ) then
                        write ( *, fmt = fmt_allocproblem ) 'deallocating'
                        write ( *, fmt = fmt_allocstat ) alloc_status
                        write ( *, fmt = fmt_allocmsg ) trim ( alloc_message )
                    end if
                end if

                ! allocate array
                allocate ( array ( 1 : rows,  1 : cols ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, fmt = fmt_allocproblem ) 'allocating'
                    write ( *, fmt = fmt_allocstat ) alloc_status
                    write ( *, fmt = fmt_allocmsg ) trim ( alloc_message )
                end if

        end subroutine allocator_rank_2_sub

        !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +                 allocator_rank_1

        module subroutine allocator_rank_1_sub ( array, rows )

            real ( rp ), allocatable, intent ( out ) :: array ( : )
            integer ( ip ),           intent ( in )  :: rows

                ! deallocate array
                if ( allocated ( array ) ) then
                    deallocate ( array, stat = alloc_status, errmsg = alloc_message )
                    if ( alloc_status /= 0 ) then
                        write ( *, fmt = fmt_allocproblem ) 'deallocating'
                        write ( *, fmt = fmt_allocstat ) alloc_status
                        write ( *, fmt = fmt_allocmsg ) trim ( alloc_message )
                    end if
                end if

                ! allocate array
                allocate ( array ( 1 : rows ), stat = alloc_status, errmsg = alloc_message )
                if ( alloc_status /= 0 ) then
                    write ( *, fmt = fmt_allocproblem ) 'allocating'
                    write ( *, fmt = fmt_allocstat ) alloc_status
                    write ( *, fmt = fmt_allocmsg ) trim ( alloc_message )
                end if

        end subroutine allocator_rank_1_sub

end module mAllocations
