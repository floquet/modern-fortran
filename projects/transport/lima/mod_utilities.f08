! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mUtilities

    use mPrecisionDefinitions,  only : ip, rp

    implicit none

    integer ( ip ), private          :: jUtil, kUtil
    integer ( ip ), private          :: io_unit, io_status

    character ( len = 512 ), private :: io_msg
    character ( len = * ), parameter :: me_module_Util = 'module mUtilities'  ! self-identification

    contains

!       ############################################################################################
!       #                                                                                          #
!       #  Export                                                                                  #
!       #                                                                                          #
!       ############################################################################################

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++       export_generic_anyrank

        subroutine export_generic_anyrank_sub ( descriptor, rank1, rank2 )

            !class ( populations ), target        :: me
            character ( len = * ), intent ( in ) :: descriptor
            real ( rp ), intent ( in ), optional :: rank1 ( : )
            real ( rp ), intent ( in ), optional :: rank2 ( : , : )

            integer ( ip )                       :: size1, size2
            character ( len = 512 )              :: file_name
            character ( len = * ), parameter     :: myRoutine = 'subroutine export_generic_anyrank_sub'  ! self-identification
            character ( len = * ), parameter     :: stop_msg = 'Execution ending due to error in ' // myRoutine // &
                                                                                              ', ' // me_module_Util

                file_name = trim ( descriptor ) // '.r64'

                if ( present ( rank2 ) ) then
                    size1 = size ( rank2, 1 )
                    size2 = size ( rank2, 2 )
                    write ( * , 100 ) size1, size2, sizeof ( rank2 ), trim ( file_name )
                    open ( newunit = io_unit, file = 'data/' // file_name, action = 'write', status = 'replace', &
                          form = 'unformatted', access = 'stream', iostat = io_status, iomsg = io_msg )
                    if ( io_status /= 0 ) then
                        write ( * , 200 ) 'OPEN', trim ( file_name ), io_unit
                        write ( * , 210 ) io_status
                        write ( * , 220 ) trim ( io_msg )
                        stop stop_msg
                    end if

                    write  ( unit = io_unit, iostat = io_status, iomsg = io_msg ) &
                                         [ ( ( rank2, jUtil = 1, size1 ), kUtil = 1, size2 ) ]
                    if ( io_status /= 0 ) then
                        write ( * , 200 ) 'WRITE', trim ( file_name ), io_unit
                        write ( * , 210 ) io_status
                        write ( * , 220 ) trim ( io_msg )
                        stop stop_msg
                    end if
                end if

                if ( present ( rank1 ) ) then
                    size1 = size ( rank1, 1 )
                    write ( * , 110 ) size1, sizeof ( rank1 ), trim ( file_name )
                    open ( newunit = io_unit, file = 'data/' // file_name, action = 'write', status = 'replace', &
                          form = 'unformatted', access = 'stream', iostat = io_status, iomsg = io_msg )
                    if ( io_status /= 0 ) then
                        write ( * , 200 ) 'OPEN', trim ( file_name ), io_unit
                        write ( * , 210 ) io_status
                        write ( * , 220 ) trim ( io_msg )
                        stop stop_msg
                    end if

                    write  ( unit = io_unit, iostat = io_status, iomsg = io_msg ) &
                                         [ ( rank1, jUtil = 1, size1 ) ]
                    if ( io_status /= 0 ) then
                        write ( * , 200 ) 'WRITE', trim ( file_name ), io_unit
                        write ( * , 210 ) io_status
                        write ( * , 220 ) trim ( io_msg )
                        stop stop_msg
                    end if
                end if

                close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( * , 200 ) 'CLOSE', trim ( file_name ), io_unit
                    write ( * , 210 ) io_status
                    write ( * , 220 ) trim ( io_msg )
                    write ( * , 230 )
                end if

                return

            100 format ( /, 'Writing array of ', g0, ' x ', g0,' elements (', g0, ' bytes at wp) to ', g0, '.' )
            110 format ( /, 'Writing array of ', g0, ' elements (', g0, ' bytes at wp) to ', g0, '.' )

            200 format ( /, g0, ' error for file "', g0, '"  on newunit ', g0 )
            210 format ( 'iostat = ', g0 )
            220 format ( 'io_msg = ', g0, '.', / )
            230 format ( 'execution contines ...', / )

        end subroutine export_generic_anyrank_sub

end module mUtilities
