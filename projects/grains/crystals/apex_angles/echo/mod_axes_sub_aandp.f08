! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
submodule ( mAxes ) smAllocPop

contains

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +     allocate_column_vectors

    module subroutine allocate_column_vectors_sub ( me )

        class ( axis ), target :: me

            ! rank 2 allocations
            call allocator_rank_2_sub ( array = me % matrix_A,        rows = me % nPoints, cols = nDof )

            ! rank 1 allocations
            call allocator_rank_1_sub ( array = me % X,               rows = me % nPoints )
            call allocator_rank_1_sub ( array = me % Y,               rows = me % nPoints )
            call allocator_rank_1_sub ( array = me % J,               rows = me % nPoints )
            call allocator_rank_1_sub ( array = me % ones,            rows = me % nPoints )
            call allocator_rank_1_sub ( array = me % residual_errors, rows = me % nPoints )

    end subroutine allocate_column_vectors_sub

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +     populate_column_vectors

    module subroutine populate_column_vectors_sub ( me, xyphi )

        class ( axis ), target              :: me
        type ( LAMMPS_data ), intent ( in ) :: xyphi

        integer ( ip )                      :: io_debug
        character ( len = 512 )             :: myFile

            me % ones = one
            me % residual_errors = zero

            myFile = trim ( me % path_case ) // 'partition member.txt'
            io_debug  = safeopen_writereplace ( filename = myFile )

            first = 1
            do kAxis = 1, me % nParts
                last = first + me % partition ( kAxis ) - 1
                write ( io_debug , '( "Partition ", g0, " has ", g0, " members (elements ", g0, " to ", g0, "):" )' ) &
                        kAxis, me % partition ( kAxis ), first, last
                write ( io_debug , * ) me % members ( first : last )
                me % J ( first : last ) = kAxis - 1
                me % X ( first : last ) = xyphi % x ( me % members ( first : last ) )
                me % Y ( first : last ) = xyphi % y ( me % members ( first : last ) )
                first = last + 1
            end do

            close ( unit = io_debug )

    end subroutine populate_column_vectors_sub

end submodule smAllocPop
