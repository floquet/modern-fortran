!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

!   INPUTS:
!       list of 1024 addresses - output from simulation

!   OUTPUTS:
!       compares computations in subroutine solve_linear_system to computation in Mathematica
!                reference notebook nb: /Users/rditldmt/Dropbox/ nb/drc/molecular dynamics/orientation/dots and numbers 05.nb
!       PASS = .TRUE.  iff all size parameters (solutions and errors) agree for all three axes (18 comparisons)

!   METHOD:
!       1. read data file from Jeff
!       2. define grain axis 1; compare Fortran results to Mathematica
!       3. define grain axis 2; compare Fortran results to Mathematica
!       4. define grain axis 3; compare Fortran results to Mathematica

!   BASICS:
!        call reader ()
!        call least_squares_solution_pts ( pts, rowCensus )
!        call grade_card ( ... )

include 'precision definitions.f90'
include 'module linear solver.f90'

program tester

    use precision_definitions, only : is, wp
    use linear_solver

    implicit NONE

!   computed by Mathematica
!   fit parameters
    real    ( wp )                     :: a01_mm = 3.437728688801653_wp
    real    ( wp )                     :: as1_mm = 0.9898939111077476_wp
    real    ( wp )                     :: a11_mm = 0.33761437784118753_wp

    real    ( wp )                     :: a02_mm = -2.07514689460362_wp
    real    ( wp )                     :: as2_mm = 4.974281484994634_wp
    real    ( wp )                     :: a12_mm = 5.168390165912342_wp

    real    ( wp )                     :: a03_mm = -7.505322556682401_wp
    real    ( wp )                     :: as3_mm = 1.2321805928043021_wp
    real    ( wp )                     :: a13_mm = -0.8576183787656415_wp

!   error in fit parameters
    real    ( wp )                     :: s01_mm = 0.013022885493030309_wp
    real    ( wp )                     :: ss1_mm = 0.0032002478858138956_wp
    real    ( wp )                     :: s11_mm = 0.0016540542769529958_wp

    real    ( wp )                     :: s02_mm = 0.09207779136468879_wp
    real    ( wp )                     :: ss2_mm = 0.05243052831571902_wp
    real    ( wp )                     :: s12_mm = 0.05215080716913731_wp

    real    ( wp )                     :: s03_mm = 0.04345443966997083_wp
    real    ( wp )                     :: ss3_mm = 0.00388468384065434_wp
    real    ( wp )                     :: s13_mm = 0.003788969817198812_wp

!   rank 1
    integer ( is ), allocatable        :: pts ( : )                    ! list of points defining a grain axis
    integer ( is ), allocatable        :: rowCensus ( : )              ! specifies how many points are on each row in the axis
!   rank 0
    integer ( is )                     :: nPoints = 0, nRows = 0

    logical ( kind ( .true. ) )        :: pass = .true.                ! did all of the tests pass?  ( err > good )

    character ( len =   * ), parameter :: me   = 'program tester'      ! Metcalf, Reid, Cohen: p. 309

!       read in total data set
        call reader ( )

!       Manually identify grain   ===   ===  ===   ===  ===   ===  ===   ===  ===   ===  ===   ===  ===   ===  ===   ===  ===   ===

!       allocate array point list on axis 1
        nPoints = 81
        nRows   = 7
        allocate ( pts ( 1 : nPoints ), stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 100 ) "integer ( is ) array pts"
            write ( *,   * ) "set membership for axis 1"
            write ( *, 110 ) nPoints
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

!       allocate row census for on axis 1
        allocate ( rowCensus ( 1 : nRows ), stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 100 ) "integer ( is ) array rowCensus"
            write ( *,   * ) "number of rows for axis 1"
            write ( *, 110 ) nPoints
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

!       AXIS 1
        pts = [ 519, 520, 522, 555, 556, 589, 590, 591, 624, 625, 551, 552, 553, 554, 587, 588, 621, 622, 623, 656, 657, 658, &
                582, 583, 584, 585, 586, 619, 620, 653, 654, 655, 688, 689, 690, 614, 615, 616, 617, 618, 651, 652, 685, 686, &
                687, 720, 721, 613, 646, 648, 649, 650, 683, 684, 717, 718, 719, 752, 753, 754, 787, 644, 645, 647, 680, 681, &
                682, 715, 716, 749, 750, 751, 784, 712, 713, 714, 747, 748, 781, 782, 783 ]
        rowCensus = [ 10, 12, 13, 12, 14, 12, 8 ]

!       bundler routine
        call least_squares_solution_pts ( pts, rowCensus )
        call grade_card ( solution, [ a01_mm, as1_mm, a11_mm], errors_solution, [ s01_mm, ss1_mm, s11_mm], pass, "axis 1" )

!       Manually identify grain   ===   ===  ===   ===  ===   ===  ===   ===  ===   ===  ===   ===  ===   ===  ===   ===  ===   ===

!       deallocations
        deallocate ( pts, stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 200 ) "integer ( is ) array pts"
            write ( *,   * ) "partition data set into rows"
            write ( *, 210 ) size ( pts )
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

        deallocate ( rowCensus, stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 200 ) "integer ( is ) array rowCensus"
            write ( *,   * ) "set membership for axis 2"
            write ( *, 210 ) size ( rowCensus )
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

!       allocate array point list on axis 2
        nPoints = 83
        nRows   = 14
        allocate ( pts ( 1 : nPoints ), stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 100 ) "integer ( is ) array pts"
            write ( *,   * ) "set membership for axis 1"
            write ( *, 110 ) nPoints
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

!       allocate row census for on axis 2
        allocate ( rowCensus ( 1 : nRows ), stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 100 ) "integer ( is ) array rowCensus"
            write ( *,   * ) "number of rows for axis 1"
            write ( *, 110 ) nPoints
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

!       AXIS 2
        pts = [ 658, 690, 787, 625, 657, 689, 721, 754, 624, 656, 688, 720, 753, 591, 623, 655, 687, 752, 784, 590, 622, 654, &
                686, 719, 751, 783, 589, 621, 653, 685, 718, 750, 782, 556, 588, 620, 652, 717, 749, 781, 555, 587, 619, 651, &
                684, 716, 748, 779, 522, 554, 586, 618, 683, 715, 747, 520, 553, 585, 617, 650, 682, 714, 519, 552, 584, 616, &
                649, 681, 713, 745, 551, 583, 615, 648, 680, 582, 614, 646, 647, 679, 613, 645, 678 ]
        rowCensus = [ 3, 5, 5, 6, 7, 7, 7, 8, 7, 7, 8, 5, 5, 3 ]

!       bundler routine
        call least_squares_solution_pts ( pts, rowCensus )
        call grade_card ( solution, [ a02_mm, as2_mm, a12_mm ], errors_solution, [ s02_mm, ss2_mm, s12_mm ], pass, "axis 2" )


!       deallocations
        deallocate ( pts, stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 200 ) "integer ( is ) array pts"
            write ( *,   * ) "partition data set into rows"
            write ( *, 210 ) size ( pts )
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

        deallocate ( rowCensus, stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 200 ) "integer ( is ) array rowCensus"
            write ( *,   * ) "set membership for axis 3"
            write ( *, 210 ) size ( rowCensus )
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

!       allocate array point list on axis 3
        nPoints = 83
        nRows   = 13
        allocate ( pts ( 1 : nPoints ), stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 100 ) "integer ( is ) array pts"
            write ( *,   * ) "set membership for axis 3"
            write ( *, 110 ) nPoints
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

!       allocate row census for on axis 3
        allocate ( rowCensus ( 1 : nRows ), stat = alloc_status, errmsg = alloc_msg )
        if ( alloc_status /= 0 ) then
            write ( *, 100 ) "integer ( is ) array rowCensus"
            write ( *,   * ) "number of rows for axis 3"
            write ( *, 110 ) nPoints
            write ( *, 120 ) alloc_status
            write ( *, 130 ) trim ( alloc_msg )
            write ( *, 140 )
            write ( *, 150 ) me
            stop
        end if

!       AXIS 3
        pts = [ 519, 551, 582, 520, 552, 583, 613, 614, 644, 522, 553, 584, 615, 645, 646, 554, 555, 585, 616, 647, 648, 678, &
                556, 586, 587, 617, 649, 679, 680, 588, 589, 618, 619, 650, 681, 712, 590, 620, 621, 651, 682, 683, 713, 591, &
                622, 652, 653, 684, 714, 715, 745, 623, 624, 654, 685, 716, 717, 747, 625, 655, 656, 686, 718, 748, 749, 657, &
                687, 688, 719, 750, 779, 781, 658, 689, 720, 751, 752, 782, 690, 721, 753, 783, 784 ]
        rowCensus = [ 3, 6, 6, 7, 7, 7, 7, 8, 7, 7, 7, 6, 5 ]

!       bundler routine
        call least_squares_solution_pts ( pts, rowCensus )
        call grade_card ( solution, [ a03_mm, as3_mm, a13_mm ], errors_solution, [ s03_mm, ss3_mm, s13_mm ], pass, "axis 3" )


        write ( *, '( /, "did all tests pass? ", g0, / )' ) pass

        stop "successful completion for " // me  ! must reduce to constant expression

  100       format ( /, "Error allocating memory for ", A )
  110       format (    "requested size is ", g0 )
  120       format (    "stat = ", g0 )
  130       format (    "errmsg = ", A, "." )
  140       format (    "Fatal error - ending run" )
  150       format (    "inside main ", A )

  200       format ( /, "Error deallocating memory for ", A )
  210       format (    "current size is ", g0 )

end program tester


!  dan-topas-pro-2:jeff rditldmt$ date
!  Fri Jun  5 14:13:17 CDT 2015
!  dan-topas-pro-2:jeff rditldmt$ pwd
!  /Users/rditldmt/SVN wd/fortran/md/codes/f08/trunk/jeff
!  dan-topas-pro-2:jeff rditldmt$ gfortran -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds linear\ solver\ tester.f90
!  dan-topas-pro-2:jeff rditldmt$ ./a.out

!  comparison with Mathematica for axis 1...
!  solution parameters...
!  offset,  alpha 0: difference = -.53290705182007514E-014, (relative error =   0.16E-14) [ value = 3.4377286888016476]
!  spacing, alpha *: difference = .22204460492503131E-015, (relative error =   0.22E-15) [ value = .98989391110774783]
!  slope,   alpha 1: difference = -.44408920985006262E-015, (relative error =   0.13E-14) [ value = .33761437784118709]

!  errors in solution parameters...
!  error in offset,  alpha 0: difference = -.86736173798840355E-017, (relative error =   0.67E-15) [ value = .13022885493030300E-001]
!  error in spacing, alpha *: difference = -.26020852139652106E-017, (relative error =   0.81E-15) [ value = .32002478858138930E-002]
!  error in slope,   alpha 1: difference = -.15178830414797062E-017, (relative error =   0.92E-15) [ value = .16540542769529942E-002]

!  comparison with Mathematica for axis 2...
!  solution parameters...
!  offset,  alpha 0: difference = -.65369931689929217E-012, (relative error =   0.32E-12) [ value = -2.0751468946042735]
!  spacing, alpha *: difference = .10231815394945443E-011, (relative error =   0.21E-12) [ value = 4.9742814849956574]
!  slope,   alpha 1: difference = .99475983006414026E-012, (relative error =   0.19E-12) [ value = 5.1683901659133369]

!  errors in solution parameters...
!  error in offset,  alpha 0: difference = .98532293435482643E-014, (relative error =   0.11E-12) [ value = .92077791364698638E-001]
!  error in spacing, alpha *: difference = .55788706987414116E-014, (relative error =   0.11E-12) [ value = .52430528315724596E-001]
!  error in slope,   alpha 1: difference = .55441762292218755E-014, (relative error =   0.11E-12) [ value = .52150807169142853E-001]

!  comparison with Mathematica for axis 3...
!  solution parameters...
!  offset,  alpha 0: difference = .14210854715202004E-013, (relative error =   0.19E-14) [ value = -7.5053225566823869]
!  spacing, alpha *: difference = .53290705182007514E-014, (relative error =   0.43E-14) [ value = 1.2321805928043075]
!  slope,   alpha 1: difference = .35527136788005009E-014, (relative error =   0.41E-14) [ value = -.85761837876563796]

!  errors in solution parameters...
!  error in offset,  alpha 0: difference = -.20816681711721685E-016, (relative error =   0.48E-15) [ value = .43454439669970812E-001]
!  error in spacing, alpha *: difference = -.30357660829594124E-017, (relative error =   0.78E-15) [ value = .38846838406543368E-002]
!  error in slope,   alpha 1: difference = -.21684043449710089E-017, (relative error =   0.57E-15) [ value = .37889698171988100E-002]

!  did all tests pass? T

!  STOP successful completion for program tester
!  dan-topas-pro-2:jeff rditldmt$
