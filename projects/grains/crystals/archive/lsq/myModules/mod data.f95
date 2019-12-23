! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mRawData

    use iso_fortran_env
    use precision_definitions, only : is, wp
    use parameters_simulation, only : numDims
    implicit none

    type                         :: packed_data
        integer ( is )           :: numPts
        real ( wp ), allocatable :: position ( 1 : maxDataPts, 1 : numDims )
        real ( wp ), allocatable :: norm ( 1 : maxDataPts )
        real ( wp ), allocatable :: phi ( 1 : maxDataPts )
        real ( wp ), allocatable :: phi_scaled ( 1 : maxDataPts )
    end type packed_data

    type               :: raw_data
        integer ( is ) :: numPts
        real ( wp )    :: position ( 1 : maxDataPts, 1 : numDims )
        real ( wp )    :: phi ( 1 : maxDataPts )
    contains
        procedure, public        :: read_triplet               => read_triplet_sub
    end type raw_data

    private :: read_triplet

    integer ( is ) :: kData
    character ( len = * ), parameter, private :: me_module_Read = 'module mRawData'  ! self-identification

    contains

!       ############################################################################################
!       #                                                                                          #
!       #  Read *.txt                                                                              #
!       #                                                                                          #
!       ############################################################################################

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                 read_triplet

        subroutine read_triplet_sub ( me, file_name, echo, timer )

            class ( raw_data ), target             :: me
            character ( len = 128 ), intent ( in ) :: file_name
            logical, optional, intent ( in )       :: echo, timer

            character ( len = * ), parameter       :: path = 'data/input/'
            character ( len = * ), parameter       :: myAction = 'read', myStatus = 'replace', myAccess = 'stream'
            character ( len = * ), parameter       :: myRoutine = 'subroutine reader'  ! self-identification
            character ( len = * ), parameter       :: stop_msg = 'Execution ending due to error in ' // myRoutine // &
                                                                                                ', ' // me_module_Util

!               open file for reading
                open ( newunit = io_unit, file = path // file_name, action = myAction, status = myStatus, &
                        access = myAccess, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( * , 200 ) 'OPEN', trim ( file_name ), io_unit
                    write ( * , 210 ) io_status
                    write ( * , 220 ) trim ( io_msg )
                    stop stop_msg
                end if

                call cpu_time ( cpu_0 ) ! cpu time - start
                read_loop : do kData = 1, maxDataPts
                    read  ( unit = io_unit, iostat = io_status, iomsg = io_msg ) position ( kData, 1 ), position ( kData, 2 ), &
                                                                                 phi ( kData )
                    if ( io_status = iostant_end ) exit !EOF
                    if ( io_status /= 0 ) then
                        write ( * , 200 ) 'READ', trim ( file_name ), io_unit
                        write ( * , 210 ) io_status
                        write ( * , 220 ) trim ( io_msg )
                        stop stop_msg
                    end if
                    numPts = kData
                end do read_loop
                call cpu_time ( cpu_1 ) ! cpu time - finish
                if present ( echo ) then
                    if ( echo ) then
                        write ( * , 100 ) nPts, path, file_name
                    end if
                end if
                if ( present ( timer ) ) if ( timer ) write ( * , 120 ) cpu_1 - cpu_0

                close ( unit = io_unit, iostat = io_status, iomsg = io_msg )
                if ( io_status /= 0 ) then
                    write ( * , 200 ) 'CLOSE', trim ( file_name ), io_unit
                    write ( * , 210 ) io_status
                    write ( * , 220 ) trim ( io_msg )
                    write ( * , 240 )
                end if

                return

    100         format ( /, 'Array of ', g0, ' elements read in from ', g0, g0, '.' )
    120         format (    'CPU time for exporting binary files = ', g0, ' seconds' )

    200         format ( /, g0, ' error for file "', g0, '"  on newunit ', g0 )
    210         format ( 'iostat = ', g0 )
    220         format ( 'io_msg = ', g0, '.', / )
    230         format ( 'Settings: action = ', g0, ', status = ', g0, ', access = ', g0, '.' )
    240         format ( 'execution contines ...', / )

        end subroutine read_triplet_sub


end module mRawData
