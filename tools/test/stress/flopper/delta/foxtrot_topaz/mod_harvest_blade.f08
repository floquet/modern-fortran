module mHarvestnode

    implicit none

contains

    subroutine node_id ( host_name, rack, icu, node )

        character ( len = * ), intent ( in )  :: host_name
        character ( len = 2 ), intent ( out ) :: rack, icu, node

        integer :: length_node, rpos, ipos, npos

            length_node = len_trim ( host_name )
            if ( length_node < 1 ) stop 'Bad host_name length; execution halts.'

            ! locate descriptors
            rpos = index ( host_name, 'r' )
            ipos = index ( host_name, 'i' )
            npos = index ( host_name, 'n' )

            ! rack
            write ( rack, '( g0 )' ) host_name ( rpos + 1 : ipos - 1 )

            ! icu
            write ( icu, '( g0 )' ) host_name ( ipos + 1 : npos - 1 )

            ! node
            write ( node, '( g0 )' ) host_name ( npos + 1 : length_node )

    end subroutine node_id

end module mHarvestnode
