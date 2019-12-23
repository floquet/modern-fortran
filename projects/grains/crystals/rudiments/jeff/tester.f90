!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! Mathematica nb: /Users/rditldmt/Dropbox/ nb/drc/molecular dynamics/orientation/fortran loader 02.nb
include 'precision definitions.f90'
include 'module solver3.f90'

program tester

    use precision_definitions, only : is, wp, zero
    use solver3

    implicit NONE
!   rank 0
    real    ( wp )                   :: diff = zero, err = zero, value = zero
!   computed by Mathematica
    real    ( wp )                   :: a1_mm = 3.437432833247386_wp
    real    ( wp )                   :: a2_mm = 0.9900280897520721_wp
    real    ( wp )                   :: a3_mm = 0.33758555330051987_wp

!   rank 1
    integer ( is ), allocatable      :: J
    integer ( is ), allocatable      :: ones
    integer ( is ), allocatable      :: rowCensus
!   rank 0
    integer ( is )                   :: k = 0, l = 0, index = 0

    character ( len = * ), parameter :: me   = 'program tester' ! Metcalf, Reid, Cohen: p. 309

        call reader ( x ( : ), y ( : ) )

!       bundler routine
!       call least_squares_solution ( X, Y, solution, errors_solution )

!       create and solve linear system
!       call left_hand_side ( X, rowCensus, ones, J )
!       call solve_linear_system ( ones, J, X, Y, solution, errors )

!       compute solution
!       call solver ( ones, J, X, Y, solution )

!       grade card
!       write ( *, '( /, "comparison with Mathematica..." )' )
!       value = solution ( 1 )
!       diff  = value - a1_mm
!       err   = diff / value
!       write ( *, 100 ) "offset,  alpha 0", diff, err, value

!       value = solution ( 2 )
!       diff  = value - a2_mm
!       err   = diff / value
!       write ( *, 100 ) "spacing, alpha 1", diff, err, value

!       value = solution ( 3 )
!       diff  = value - a3_mm
!       err   = diff / value
!       write ( *, 100 ) "slope,   alpha 2", diff, err, value

        stop "successful completion for " // me  ! must reduce to constant expression

!  100   format ( A, ": difference = ", g0, ", (relative error = ", E10.2, ") - value = ", g0 )

end program tester


!  11:07 Dan-Topas-Mac-Pro-2 rditldmt $ pwd
!  /Users/rditldmt/SVN wd/fortran/md/codes/f08/trunk/grains
!  11:07 Dan-Topas-Mac-Pro-2 rditldmt $ gfortran -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds tester.f90
!  11:07 Dan-Topas-Mac-Pro-2 rditldmt $ ./a.out
!
!  comparison with Mathematica...
!  offset,  alpha 0: difference = .26645352591003757E-014, (relative error =   0.78E-15) - value = 3.4374328332473887
!  spacing, alpha 1: difference = -.24424906541753444E-014, (relative error =  -0.25E-14) - value = .99002808975206968
!  slope,   alpha 2: difference = -.44408920985006262E-015, (relative error =  -0.13E-14) - value = .33758555330051943
!  STOP successful completion for program tester