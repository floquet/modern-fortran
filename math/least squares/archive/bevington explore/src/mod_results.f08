! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 30

module mResults

    use mPrecisionDefinitions,  only : ip, rp
    use mParameters,            only : one
    use mMeasurements
    use mSolnsLinear
    implicit none

    type                       :: results
        type ( measurements )  :: measurement
        type ( solns_linear )  :: soln_linear ( 1 : 5 )
        integer ( ip )         :: numSolnsLinear
        character ( len = 64 ) :: descriptor_64
    contains
        private
        ! subroutines
        procedure, public :: print_measurement   => print_measurement_sub
        procedure, public :: print_one_solution  => print_one_solution_sub
        procedure, public :: print_all_solutions => print_all_solutions_sub
    end type results

    integer ( ip ), private                   :: kResult = 0
    character ( len = * ), parameter, private :: myModule = 'module mResults'  ! self-identification

    private :: print_measurement_sub
    private :: print_one_solution_sub
    private :: print_all_solutions_sub

    contains

!       =============================================================================================              print_measurement

        subroutine print_measurement_sub ( me )

            class ( results ), target :: me

!               headers
                write ( * , 100 ) me % descriptor_64
                write ( * , 110 ) me % measurement % descriptor_64

!               print data
                do kResult = 1, me % measurement % m
                    write ( * , 200 ) kResult, me % measurement % x ( kResult ), me % measurement % y ( kResult )
                end do

            return

    100     format ( /, g0 )
    110     format (    g0, / )

    200     format ( I5, '. ', T10, g0, T30, g0 )

        end subroutine print_measurement_sub

!       =============================================================================================             print_one_solution

        subroutine print_one_solution_sub ( me, who )

            class ( results ), target      :: me
            integer ( ip ), intent ( in )  :: who

            type ( solns_linear ), pointer :: pSoln

                pSoln => me % soln_linear ( who )
!                   headers
                    write ( * , 100 ) me % descriptor_64
                    write ( * , 110 ) pSoln % descriptor_64

                    write ( * , 200 ) 'slope:     ', pSoln % solution ( 1 ), pSoln % error ( 1 )
                    write ( * , 200 ) 'intercept: ', pSoln % solution ( 2 ), pSoln % error ( 2 )

!                   check for warnings
                    if ( status /= 0 ) then
                        write ( * , 300 ) pSoln % status
                        write ( * , 310 ) trim ( warning )
                    end if
                pSoln => null ( )

            return

    100     format ( /, g0 )
    110     format (    g0, / )

    200     format ( g0, g0, ' +/- ', g0 )

    300     format ( 'status = ', g0 )
    310     format ( g0, '.' )

        end subroutine print_one_solution_sub

!       =============================================================================================            print_all_solutions

        subroutine print_all_solutions_sub ( me )

            class ( results ), target :: me

                do kResult = 1, me % numSolnsLinear
                    call me % print_one_solution ( kResult )
                end do

        end subroutine print_all_solutions_sub

end module mResults
