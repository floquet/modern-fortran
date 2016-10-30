module mHarvestBlade

    implicit none

contains

    subroutine blade_id ( host_name, rack, icu, blade )

        character ( len = * ), intent ( in )  :: host_name
        character ( len = 2 ), intent ( out ) :: rack, icu, blade

        integer :: length_blade, rpos, ipos, npos

            length_blade = len_trim ( host_name )
            if ( length_blade < 1 ) stop 'Bad host_name length; execution halts.'

            ! locate descriptors
            rpos = index ( host_name, 'r' )
            ipos = index ( host_name, 'i' )
            npos = index ( host_name, 'n' )

            ! rack
            write ( rack, '( g0 )' ) host_name ( rpos + 1 : ipos - 1 )
            ! length = len_trim ( rStblade_idg )
            ! if ( length == 1 ) rStblade_idg = '0' // trim ( rStblade_idg )

            ! icu
            write ( icu, '( g0 )' ) host_name ( ipos + 1 : npos - 1 )
            ! length = len_trim ( iStblade_idg )
            ! if ( length == 1 ) iStblade_idg = '0' // trim ( iStblade_idg )

            ! blade
            write ( blade, '( g0 )' ) host_name ( npos + 1 : length_blade )
            ! length = len_trim ( nStblade_idg )
            ! if ( length == 1 ) nStblade_idg = '0' // trim ( nStblade_idg )

    end subroutine blade_id

end module mHarvestBlade
