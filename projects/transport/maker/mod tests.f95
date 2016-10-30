! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mTests

    !use precision_definitions, only : is, wp, zero, one
    use mRandom
    use mQueries

    implicit none

    integer, private :: kTest

    contains                                                                                    ! methods: subroutines and functions

!       +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            test_random_tools

        subroutine test_random_tools ( test_nballs, test_vectors )

            integer ( is )                   :: numArgsInput
            integer ( is )                   :: c_arg_list ( 1 : numArgsMax )

            logical, intent ( in ), optional :: test_vectors
            logical, intent ( in ), optional :: test_nballs

            integer ( is )                   :: nDim = 0, nRotations = 0

            real    ( wp ), allocatable      :: A ( : , : )
            real    ( wp )                   :: v ( 1 : 2 )

                if ( present ( test_vectors ) ) then
                    if ( test_vectors .eqv. .true. ) then
                        write ( * , 100 )
                        v = random_direction_fcn ( )
                        write ( * , 110 ) v
                    end if
                end if

                if ( present ( test_nballs ) ) then
                    if ( test_nballs .eqv. .true. ) then
                        write ( * , 200 )
                        call harvest_command_line_arguments ( c_arg_list, numArgsInput )
                        if ( numArgsInput >= 2 ) then
                            nDim = c_arg_list ( 1 )
                            nRotations = c_arg_list ( 2 )
                        else
                            nDim = 5
                            nRotations = 100
                        endif

                        write ( * , 210 ) nDim, nRotations

                        A = unit_ball ( nDim, nRotations )
                        do kTest = 1, nDim
                            write ( * , '( "row ", g0, " = ", 100F9.3 )' ) kTest, A ( : , kTest )
                        end do

                    end if
                end if

                return

  100           format ( /, 'testing random vectors...' )
  110           format (    'random vector = ', g0 )

  200           format ( /, 'testing random vectors on the unit sphere...' )
  210           format (    'matrix dimension = ', g0, ', ', 'number of rotations = ', g0 )

        end subroutine test_random_tools

end module mTests
