! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mAxes

    use mPrecisionDefinitions,         only : ip, rp
    use mConstants,                    only : zero, one

    use mAllocations,                  only : allocator_rank_1_ints_sub, allocator_rank_1_sub, allocator_rank_2_sub
    use mFileHandling,                 only : safeopen_writereplace
    use mLSQ,                          only : lsq_fit
    use mParametersSimulation,         only : path_data, nDof
    use mReadLAMMPS,                   only : LAMMPS_data

    implicit none

    ! rank 2
    real ( rp ),    private :: Winv ( 1 : nDof, 1 : nDof )
    ! rank 1
    real ( rp ),    private :: solution ( 1 : nDof )

    integer ( ip ), private :: kAxis
    integer ( ip ), private :: first, last ! not used here, but in both submodules

    type :: axis
        integer ( ip )              :: nParts = 0, nPoints = 0
        integer ( ip ), allocatable :: partition ( : ) ! number of points in each row
        integer ( ip ), allocatable :: members   ( : ) ! list of points in all rows
        type ( lsq_fit )            :: results
        real ( rp )                 :: phi_total = zero, phi_mean = zero
        real ( rp ),    allocatable :: matrix_A ( : , : )
        real ( rp ),    allocatable :: X ( : ), Y ( : ), ones ( : ), J ( : ), residual_errors ( : )
        character ( len = 8 )       :: iam = ''
        character ( len = 512 )     :: path_case = ''
    contains
        private
        procedure, public :: read_members               => read_members_sub
        procedure, public :: read_partition             => read_partition_sub
        procedure, public :: export_solution            => export_solution_sub
        procedure, public :: error_propagation          => error_propagation_sub
        procedure, public :: least_squares_solution     => least_squares_solution_sub
        procedure, public :: allocate_column_vectors    => allocate_column_vectors_sub
        procedure, public :: populate_column_vectors    => populate_column_vectors_sub
    end type axis

    private :: read_members_sub
    private :: read_partition_sub
    private :: export_solution_sub
    private :: error_propagation_sub
    private :: least_squares_solution_sub
    private :: allocate_column_vectors_sub
    private :: populate_column_vectors_sub

    interface
        module subroutine export_solution_sub ( me )
            class ( axis ), target  :: me
        end subroutine export_solution_sub

        module subroutine read_members_sub ( me )
            class ( axis ), target  :: me
        end subroutine read_members_sub

        module subroutine read_partition_sub ( me )
            class ( axis ), target  :: me
        end subroutine read_partition_sub

        module subroutine allocate_column_vectors_sub ( me )
            class ( axis ), target :: me
        end subroutine allocate_column_vectors_sub

        module subroutine populate_column_vectors_sub ( me, xyphi )
            class ( axis ), target              :: me
            type ( LAMMPS_data ), intent ( in ) :: xyphi
        end subroutine populate_column_vectors_sub
    end interface

contains

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +   least_squares_solution

    subroutine least_squares_solution_sub ( me, iam, subdir_case, xyphi )

        class ( axis ), target                 :: me
        type ( LAMMPS_data ),  intent ( in )   :: xyphi ! LAMMPS data
        character ( len = * ), intent ( in )   :: iam, subdir_case

            me % iam = trim ( iam ) ! e.g. 'axis I'
            me % path_case = path_data // subdir_case

            call read_partition_sub ( me )
            call read_members_sub ( me )
            call allocate_column_vectors_sub ( me )
            call populate_column_vectors_sub ( me, xyphi )
            call solve_linear_system_sub ( me )
            call error_propagation_sub ( me )
            call export_solution_sub ( me )

    end subroutine least_squares_solution_sub

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +   solve_linear_system

    subroutine solve_linear_system_sub ( me )

        use mMatrixWriter, only : open_file_print_matrix

        class ( axis ), target      :: me

        ! rank 1
        real ( rp ) :: beta ( 1 : nDof ) = zero
        ! rank 0
        real ( rp ) :: a = zero, b = zero, c = zero, d = zero, e = zero, f = zero
        real ( rp ) :: det = zero

            ! initializations
            Winv ( : , : ) = zero
            solution ( : ) = zero

            ! unique elements in product matrix A'A
            a = dot_product ( me % ones, me % ones )
            b = dot_product ( me % ones, me % J )
            c = dot_product ( me % ones, me % X )
            d = dot_product ( me % J, me % J )
            e = dot_product ( me % J, me % X )
            f = dot_product ( me % X, me % X )

            ! determinant
            det = 2 * b * c * e + a * d * f - a * e ** 2 - d * c ** 2 - f * b ** 2

            ! data vector
            beta ( 1 ) = dot_product ( me % ones, me % Y )
            beta ( 2 ) = dot_product ( me % J,    me % Y )
            beta ( 3 ) = dot_product ( me % X,    me % Y )

            ! populate unique elements in inverse matrix
            Winv (   :  , 1 ) = [ d * f - e ** 2, c * e - b * f,  b * e - c * d ]
            Winv ( 2 : 3, 2 ) = [ a * f - c ** 2, b * c - a * e ]
            Winv ( 3 : 3, 3 ) = [ a * d - b ** 2 ]

            ! populate repeated elements using symmetry
            Winv ( 1, 2 )    = Winv ( 2, 1 )
            Winv ( 1, nDof ) = Winv ( 3, 1 )
            Winv ( 2, 3 )    = Winv ( 3, 2 )

            Winv = Winv / det                                              ! inverse matrix
            call open_file_print_matrix ( A = Winv, myFormat = 'E10.3', spaces = 2, moniker = 'Winv', &
                                          myFile = trim ( me % path_case ) // 'Winv.txt' )

            solution = matmul ( Winv, beta )                               ! solution vector

            ! map vector to explicit names
            me % results % gap       = solution ( 1 )
            me % results % intercept = solution ( 1 )
            me % results % slope     = solution ( 3 )

            me % matrix_A ( : , 1 ) = me % ones
            me % matrix_A ( : , 2 ) = me % J
            me % matrix_A ( : , 3 ) = me % X
            call open_file_print_matrix ( A = me % matrix_A, myFormat = 'F10.3', spaces = 2, moniker = 'System matrix A', &
                                          myFile = trim ( me % path_case ) // 'A matrix.txt' )

    end subroutine solve_linear_system_sub

    !  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  error_propagation

    subroutine error_propagation_sub ( me )

        class ( axis ), target      :: me

        ! rank 1
        real ( rp ) :: solution_errors ( 1 : nDof ) = zero
        ! rank 0
        real ( rp ) :: SSE = zero

            me % residual_errors = matmul ( me % matrix_A, solution ) - me % Y
            SSE = dot_product ( me % residual_errors, me % residual_errors ) ! sum of squared errors

            ! Data analysis and error analysis for the physical sciences, 1e
            ! Philip Bevingtion, section 6.4 "Estimation of errors"
            solution_errors = [ ( Winv ( kAxis, kAxis ), kAxis = 1, nDof ) ]  ! diagonal elements of Winv
            ! scale by estimated parent standard deviation ( SSE )
            solution_errors = solution_errors * SSE / real( me % nPoints - 3, rp )
            solution_errors = sqrt ( solution_errors )

            me % results % err_gap       = solution_errors ( 1 )
            me % results % err_intercept = solution_errors ( 2 )
            me % results % err_slope     = solution_errors ( 3 )

    end subroutine error_propagation_sub

end module mAxes
