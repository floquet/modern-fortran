! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 09 24

module mValidate

    use mDataType
    implicit none

    integer ( is ), parameter :: nBevington = 9
    real ( rp )               :: measure ( 1 : nBevington )
    type ( data )             :: bevington_data

    contains                                                                                    ! methods: subroutines and functions

!       ===========================================================================================  load_bevington_measurements_sub

        subroutine load_bevington_measurements_sub ()

            measure ( 1 ) = [ 1.0_rp, 15.6_rp ]
            measure ( 2 ) = [ 2.0_rp, 17.5_rp ]
            measure ( 3 ) = [ 3.0_rp, 36.6_rp ]
            measure ( 4 ) = [ 4.0_rp, 43.8_rp ]
            measure ( 5 ) = [ 5.0_rp, 58.2_rp ]
            measure ( 6 ) = [ 6.0_rp, 61.6_rp ]
            measure ( 7 ) = [ 7.0_rp, 64.2_rp ]
            measure ( 8 ) = [ 8.0_rp, 70.4_rp ]
            measure ( 9 ) = [ 9.0_rp, 98.8_rp ]

        end subroutine load_bevington_measurements_sub ()

!       ===========================================================================================          validation_exercise_sub

        subroutine validation_exercise_sub ()

            bevington_data % numData = nBevington
            call bevington_data % allocate_group_sub ( echo = .true. )
            call load_bevington_measurements_sub ()

            measure ( 1 ) = [ 1.0_rp, 15.6_rp ]
            measure ( 2 ) = [ 2.0_rp, 17.5_rp ]
            measure ( 3 ) = [ 3.0_rp, 36.6_rp ]
            measure ( 4 ) = [ 4.0_rp, 43.8_rp ]
            measure ( 5 ) = [ 5.0_rp, 58.2_rp ]
            measure ( 6 ) = [ 6.0_rp, 61.6_rp ]
            measure ( 7 ) = [ 7.0_rp, 64.2_rp ]
            measure ( 8 ) = [ 8.0_rp, 70.4_rp ]
            measure ( 9 ) = [ 9.0_rp, 98.8_rp ]

        end subroutine validation_exercise_sub ()

    end module mValidate
