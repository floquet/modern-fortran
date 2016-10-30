!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! Mathematica nb: /Users/rditldmt/Dropbox/ nb/drc/molecular dynamics/orientation/fortran loader 02.nb
include 'precision definitions.f90'
include 'module tester.f90'

program test_module

!    use precision_definitions, only : is, wp, zero
    use tester

    implicit NONE

    character ( len = * ), parameter :: me   = 'program test_module' ! Metcalf, Reid, Cohen: p. 309

        call task ( )
        call inflate ( )
        call handoff ( )

        write ( *, '( /, "privacy test:" )' )
        write ( *, '(    "x = ", g0 )'    ) x
!        write ( *, '(    "y = ", g0, / )' ) y

        stop "successful completion for " // me  ! must reduce to constant expression

end program test_module