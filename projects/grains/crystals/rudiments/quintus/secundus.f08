! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! respect load order
! include '../../sharedModules/mod precision definitions.f08'
!
! include 'myModules/mod parameters simulation.f08'
! include 'myModules/mod format descriptors.f08'
! include 'myModules/mod shared.f08'
! include 'myModules/mod matrix writer.f08'
! include 'myModules/mod read LAMMPS.f08'
! include 'myModules/mod axes.f08'
! include 'myModules/mod mathematica results.f08'
! include 'myModules/mod grain.f08'

program secundus

!   order irrelevant
    use iso_fortran_env
    use mPrecisionDefinitions
    use mShared
    use mReadLAMMPS
    use mAxes
    use mValidateLines
    use mValidateApex
    use mGrain

    implicit none

    integer ( ip )              :: k = 0
    !
    type ( LAMMPS_data )        :: myMeasurements
    type ( axis )               :: axisI, axisII, axisIII
    type ( comparison_lines )   :: setI,  setII,  setIII
    type ( comparison_apex )    :: apex_angles
    type ( grain )              :: myGrain

    character ( len = * ), parameter :: myProgram = 'program secundus'  ! self-identification
    character ( len = * ), parameter :: c_version = compiler_version ( )
    character ( len = * ), parameter :: c_options = compiler_options ( )

        ! read LAMMPS data
        call myMeasurements % read_data ( )

        ! analyze three axes for a grain
        call axisI   % least_squares_solution ( 'axis I',   'primus/',   myMeasurements )
        call axisII  % least_squares_solution ( 'axis II',  'secundus/', myMeasurements )
        call axisIII % least_squares_solution ( 'axis III', 'tertius/',  myMeasurements )

        ! measure angles
        call myGrain % compute_apex_angles ( axisI, axisII, axisIII )
        ! compare apex to Mathematica
        do k = 1, 3
            call apex_angles % validate_apex ( myGrain % apex_angle ( k ), k )
        end do
        !call myGrain % compare_mm_apex ( 2 )
        !call myGrain % compare_mm_apex ( 3 )

        ! compare grain axes to Mathematica
        call setI   % validate_lines ( axisI   % results, 1 )
        call setII  % validate_lines ( axisII  % results, 2 )
        call setIII % validate_lines ( axisIII % results, 3 )

        write ( *, '( /, "compiler version  = ", g0, "." )' ) c_version
        write ( *, '( "compiler options  = ", g0, ".", / )' ) c_options

        stop 'successful completion for ' // myProgram // '.'  ! string must reduce to constant expression

end program secundus

!  17:19 dan-topas-pro-2 rditldmt $ date
! Mon Feb  1 17:19:55 CST 2016
!  17:19 dan-topas-pro-2 rditldmt $ pwd
! /Users/rditldmt/Box Sync/fortran/projects/crystals/rudiments/quintus
!  17:19 dan-topas-pro-2 rditldmt $ scons
! scons: Reading SConscript files ...
! scons: done reading SConscript files.
! scons: Building targets ...
! scons: `.' is up to date.
! scons: done building targets.
!  17:20 dan-topas-pro-2 rditldmt $ aa
!
! Array of 1024 elements read in from ../../data/input/apple/xy_data.txt.
!
! Array of 1024 elements read in from ../../data/input/apple/potentials.txt.
!
! Array of 7 elements read in from ../../data/input/apple/primus/partition.txt.
!
! Partitions:
!   10    12    13    12    14    12     8
! Total number of points = 81
!
! Array of 81 elements read in from ../../data/input/apple/primus/members.txt.
!
! Points:
!  519   520   522   555   556   589   590   591   624   625
!  551   552   553   554   587   588   621   622   623   656   657   658
!  582   583   584   585   586   619   620   653   654   655   688   689   690
!  614   615   616   617   618   651   652   685   686   687   720   721
!  613   646   648   649   650   683   684   717   718   719   752   753   754   787
!  644   645   647   680   681   682   715   716   749   750   751   784
!  712   713   714   747   748   781   782   783
! Total number of points = 81
!
! Partition 1 has 10 members:
!          519         520         522         555         556         589         590         591         624         625
! Partition 2 has 12 members:
!          551         552         553         554         587         588         621         622         623         656         657         658
! Partition 3 has 13 members:
!          582         583         584         585         586         619         620         653         654         655         688         689         690
! Partition 4 has 12 members:
!          614         615         616         617         618         651         652         685         686         687         720         721
! Partition 5 has 14 members:
!          613         646         648         649         650         683         684         717         718         719         752         753         754         787
! Partition 6 has 12 members:
!          644         645         647         680         681         682         715         716         749         750         751         784
! Partition 7 has 8 members:
!          712         713         714         747         748         781         782         783
!
! Winv has 3 rows and 3 columns.
!  0.594E-01  -0.921E-02   0.400E-02
! -0.921E-02   0.359E-02   0.267E-03
!  0.400E-02   0.267E-03   0.958E-03
!
! System matrix A has 81 rows and 3 columns.
!      1.000       0.000      -9.178
!      1.000       0.000      -8.127
!      1.000       0.000      -7.086
!      1.000       0.000      -6.087
!      1.000       0.000      -4.956
!      1.000       0.000      -4.067
!      1.000       0.000      -2.996
!      1.000       0.000      -1.981
!      1.000       0.000      -0.957
!      1.000       0.000       0.088
!      1.000       1.000      -9.943
!      1.000       1.000      -8.925
!      1.000       1.000      -7.865
!      1.000       1.000      -6.869
!      1.000       1.000      -5.813
!      1.000       1.000      -4.824
!      1.000       1.000      -3.797
!      1.000       1.000      -2.782
!      1.000       1.000      -1.759
!      1.000       1.000      -0.698
!      1.000       1.000       0.354
!      1.000       1.000       1.386
!      1.000       2.000     -10.781
!      1.000       2.000      -9.767
!      1.000       2.000      -8.703
!      1.000       2.000      -7.704
!      1.000       2.000      -6.658
!      1.000       2.000      -5.642
!      1.000       2.000      -4.638
!      1.000       2.000      -3.619
!      1.000       2.000      -2.557
!      1.000       2.000      -1.515
!      1.000       2.000      -0.528
!      1.000       2.000       0.513
!      1.000       2.000       1.498
!      1.000       3.000     -10.634
!      1.000       3.000      -9.576
!      1.000       3.000      -8.514
!      1.000       3.000      -7.521
!      1.000       3.000      -6.485
!      1.000       3.000      -5.459
!      1.000       3.000      -4.412
!      1.000       3.000      -3.404
!      1.000       3.000      -2.381
!      1.000       3.000      -1.353
!      1.000       3.000      -0.314
!      1.000       3.000       0.719
!      1.000       4.000     -11.474
!      1.000       4.000     -10.454
!      1.000       4.000      -9.324
!      1.000       4.000      -8.338
!      1.000       4.000      -7.328
!      1.000       4.000      -6.280
!      1.000       4.000      -5.226
!      1.000       4.000      -4.216
!      1.000       4.000      -3.204
!      1.000       4.000      -2.168
!      1.000       4.000      -1.123
!      1.000       4.000      -0.109
!      1.000       4.000       1.009
!      1.000       4.000       2.000
!      1.000       5.000     -12.346
!      1.000       5.000     -11.306
!      1.000       5.000     -10.142
!      1.000       5.000      -9.141
!      1.000       5.000      -8.162
!      1.000       5.000      -7.103
!      1.000       5.000      -6.098
!      1.000       5.000      -5.073
!      1.000       5.000      -4.033
!      1.000       5.000      -3.010
!      1.000       5.000      -1.945
!      1.000       5.000      -0.911
!      1.000       6.000      -9.006
!      1.000       6.000      -7.971
!      1.000       6.000      -6.944
!      1.000       6.000      -5.852
!      1.000       6.000      -4.845
!      1.000       6.000      -3.840
!      1.000       6.000      -2.760
!      1.000       6.000      -1.714
!
! Array of 14 elements read in from ../../data/input/apple/secundus/partition.txt.
!
! Partitions:
!    3     5     5     6     7     7     7     8     7     7     8     5     5     3
! Total number of points = 83
!
! Array of 83 elements read in from ../../data/input/apple/secundus/members.txt.
!
! Points:
!  658   690   787
!  625   657   689   721   754
!  624   656   688   720   753
!  591   623   655   687   752   784
!  590   622   654   686   719   751   783
!  589   621   653   685   718   750   782
!  556   588   620   652   717   749   781
!  555   587   619   651   684   716   748   779
!  522   554   586   618   683   715   747
!  520   553   585   617   650   682   714
!  519   552   584   616   649   681   713   745
!  551   583   615   648   680
!  582   614   646   647   679
!  613   645   678
! Total number of points = 83
!
! Partition 1 has 3 members:
!          658         690         787
! Partition 2 has 5 members:
!          625         657         689         721         754
! Partition 3 has 5 members:
!          624         656         688         720         753
! Partition 4 has 6 members:
!          591         623         655         687         752         784
! Partition 5 has 7 members:
!          590         622         654         686         719         751         783
! Partition 6 has 7 members:
!          589         621         653         685         718         750         782
! Partition 7 has 7 members:
!          556         588         620         652         717         749         781
! Partition 8 has 8 members:
!          555         587         619         651         684         716         748         779
! Partition 9 has 7 members:
!          522         554         586         618         683         715         747
! Partition 10 has 7 members:
!          520         553         585         617         650         682         714
! Partition 11 has 8 members:
!          519         552         584         616         649         681         713         745
! Partition 12 has 5 members:
!          551         583         615         648         680
! Partition 13 has 5 members:
!          582         614         646         647         679
! Partition 14 has 3 members:
!          613         645         678
!
! Winv has 3 rows and 3 columns.
!  0.251E+00  -0.132E+00  -0.126E+00
! -0.132E+00   0.812E-01   0.803E-01
! -0.126E+00   0.803E-01   0.804E-01
!
! System matrix A has 83 rows and 3 columns.
!      1.000       0.000       1.386
!      1.000       0.000       1.498
!      1.000       0.000       2.000
!      1.000       1.000       0.088
!      1.000       1.000       0.354
!      1.000       1.000       0.513
!      1.000       1.000       0.719
!      1.000       1.000       1.009
!      1.000       2.000      -0.957
!      1.000       2.000      -0.698
!      1.000       2.000      -0.528
!      1.000       2.000      -0.314
!      1.000       2.000      -0.109
!      1.000       3.000      -1.981
!      1.000       3.000      -1.759
!      1.000       3.000      -1.515
!      1.000       3.000      -1.353
!      1.000       3.000      -1.123
!      1.000       3.000      -0.911
!      1.000       4.000      -2.996
!      1.000       4.000      -2.782
!      1.000       4.000      -2.557
!      1.000       4.000      -2.381
!      1.000       4.000      -2.168
!      1.000       4.000      -1.945
!      1.000       4.000      -1.714
!      1.000       5.000      -4.067
!      1.000       5.000      -3.797
!      1.000       5.000      -3.619
!      1.000       5.000      -3.404
!      1.000       5.000      -3.204
!      1.000       5.000      -3.010
!      1.000       5.000      -2.760
!      1.000       6.000      -4.956
!      1.000       6.000      -4.824
!      1.000       6.000      -4.638
!      1.000       6.000      -4.412
!      1.000       6.000      -4.216
!      1.000       6.000      -4.033
!      1.000       6.000      -3.840
!      1.000       7.000      -6.087
!      1.000       7.000      -5.813
!      1.000       7.000      -5.642
!      1.000       7.000      -5.459
!      1.000       7.000      -5.226
!      1.000       7.000      -5.073
!      1.000       7.000      -4.845
!      1.000       7.000      -4.633
!      1.000       8.000      -7.086
!      1.000       8.000      -6.869
!      1.000       8.000      -6.658
!      1.000       8.000      -6.485
!      1.000       8.000      -6.280
!      1.000       8.000      -6.098
!      1.000       8.000      -5.852
!      1.000       9.000      -8.127
!      1.000       9.000      -7.865
!      1.000       9.000      -7.704
!      1.000       9.000      -7.521
!      1.000       9.000      -7.328
!      1.000       9.000      -7.103
!      1.000       9.000      -6.944
!      1.000      10.000      -9.178
!      1.000      10.000      -8.925
!      1.000      10.000      -8.703
!      1.000      10.000      -8.514
!      1.000      10.000      -8.338
!      1.000      10.000      -8.162
!      1.000      10.000      -7.971
!      1.000      10.000      -7.730
!      1.000      11.000      -9.943
!      1.000      11.000      -9.767
!      1.000      11.000      -9.576
!      1.000      11.000      -9.324
!      1.000      11.000      -9.141
!      1.000      12.000     -10.781
!      1.000      12.000     -10.634
!      1.000      12.000     -10.454
!      1.000      12.000     -10.142
!      1.000      12.000      -9.974
!      1.000      13.000     -11.474
!      1.000      13.000     -11.306
!      1.000      13.000     -10.949
!
! Array of 13 elements read in from ../../data/input/apple/tertius/partition.txt.
!
! Partitions:
!    3     6     6     7     7     7     7     8     7     7     7     6     5
! Total number of points = 83
!
! Array of 83 elements read in from ../../data/input/apple/tertius/members.txt.
!
! Points:
!  519   551   582
!  520   552   583   613   614   644
!  522   553   584   615   645   646
!  554   555   585   616   647   648   678
!  556   586   587   617   649   679   680
!  588   589   618   619   650   681   712
!  590   620   621   651   682   683   713
!  591   622   652   653   684   714   715   745
!  623   624   654   685   716   717   747
!  625   655   656   686   718   748   749
!  657   687   688   719   750   779   781
!  658   689   720   751   752   782
!  690   721   753   783   784
! Total number of points = 83
!
! Partition 1 has 3 members:
!          519         551         582
! Partition 2 has 6 members:
!          520         552         583         613         614         644
! Partition 3 has 6 members:
!          522         553         584         615         645         646
! Partition 4 has 7 members:
!          554         555         585         616         647         648         678
! Partition 5 has 7 members:
!          556         586         587         617         649         679         680
! Partition 6 has 7 members:
!          588         589         618         619         650         681         712
! Partition 7 has 7 members:
!          590         620         621         651         682         683         713
! Partition 8 has 8 members:
!          591         622         652         653         684         714         715         745
! Partition 9 has 7 members:
!          623         624         654         685         716         717         747
! Partition 10 has 7 members:
!          625         655         656         686         718         748         749
! Partition 11 has 7 members:
!          657         687         688         719         750         779         781
! Partition 12 has 6 members:
!          658         689         720         751         752         782
! Partition 13 has 5 members:
!          690         721         753         783         784
!
! Winv has 3 rows and 3 columns.
!  0.614E+00  -0.531E-01   0.512E-01
! -0.531E-01   0.490E-02  -0.426E-02
!  0.512E-01  -0.426E-02   0.467E-02
!
! System matrix A has 83 rows and 3 columns.
!      1.000       0.000      -9.178
!      1.000       0.000      -9.943
!      1.000       0.000     -10.781
!      1.000       1.000      -8.127
!      1.000       1.000      -8.925
!      1.000       1.000      -9.767
!      1.000       1.000     -11.474
!      1.000       1.000     -10.634
!      1.000       1.000     -12.346
!      1.000       2.000      -7.086
!      1.000       2.000      -7.865
!      1.000       2.000      -8.703
!      1.000       2.000      -9.576
!      1.000       2.000     -11.306
!      1.000       2.000     -10.454
!      1.000       3.000      -6.869
!      1.000       3.000      -6.087
!      1.000       3.000      -7.704
!      1.000       3.000      -8.514
!      1.000       3.000     -10.142
!      1.000       3.000      -9.324
!      1.000       3.000     -10.949
!      1.000       4.000      -4.956
!      1.000       4.000      -6.658
!      1.000       4.000      -5.813
!      1.000       4.000      -7.521
!      1.000       4.000      -8.338
!      1.000       4.000      -9.974
!      1.000       4.000      -9.141
!      1.000       5.000      -4.824
!      1.000       5.000      -4.067
!      1.000       5.000      -6.485
!      1.000       5.000      -5.642
!      1.000       5.000      -7.328
!      1.000       5.000      -8.162
!      1.000       5.000      -9.006
!      1.000       6.000      -2.996
!      1.000       6.000      -4.638
!      1.000       6.000      -3.797
!      1.000       6.000      -5.459
!      1.000       6.000      -7.103
!      1.000       6.000      -6.280
!      1.000       6.000      -7.971
!      1.000       7.000      -1.981
!      1.000       7.000      -2.782
!      1.000       7.000      -4.412
!      1.000       7.000      -3.619
!      1.000       7.000      -5.226
!      1.000       7.000      -6.944
!      1.000       7.000      -6.098
!      1.000       7.000      -7.730
!      1.000       8.000      -1.759
!      1.000       8.000      -0.957
!      1.000       8.000      -2.557
!      1.000       8.000      -3.404
!      1.000       8.000      -5.073
!      1.000       8.000      -4.216
!      1.000       8.000      -5.852
!      1.000       9.000       0.088
!      1.000       9.000      -1.515
!      1.000       9.000      -0.698
!      1.000       9.000      -2.381
!      1.000       9.000      -3.204
!      1.000       9.000      -4.845
!      1.000       9.000      -4.033
!      1.000      10.000       0.354
!      1.000      10.000      -1.353
!      1.000      10.000      -0.528
!      1.000      10.000      -2.168
!      1.000      10.000      -3.010
!      1.000      10.000      -4.633
!      1.000      10.000      -3.840
!      1.000      11.000       1.386
!      1.000      11.000       0.513
!      1.000      11.000      -0.314
!      1.000      11.000      -1.945
!      1.000      11.000      -1.123
!      1.000      11.000      -2.760
!      1.000      12.000       1.498
!      1.000      12.000       0.719
!      1.000      12.000      -0.109
!      1.000      12.000      -1.714
!      1.000      12.000      -0.911
!
! Apex angles (ideal answer = 60 deg):
!   1     1.04032 +/-    0.01801     (  59.60592 +/-    1.03174 ) degrees
!   2     1.03450 +/-    0.00293     (  59.27242 +/-    0.16798 ) degrees
!   3     1.04138 +/-    0.01812     (  59.66650 +/-    1.03828 ) degrees
q! STOP successful completion for program secundus.
