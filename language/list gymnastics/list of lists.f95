!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
! begging for recursion
include 'modules/mod precision definitions.f90'

program lister

    use precision_definitions, only : is, wp, zero

    implicit NONE

    integer ( is ), parameter        :: n = 3
    integer ( is )                   :: i = 0, j = 0, k = 0
    integer ( is )                   :: A ( 1 : n,    1 : 1 ) = 0
    integer ( is )                   :: B ( 1 : n**2, 1 : 2 ) = 0
    integer ( is )                   :: C ( 1 : 2, : , : )    = 0
    real    ( wp )                   :: cpu_0 = zero, cpu_1 = zero, t_0 = zero, t_1 = zero

    character ( len = * ), parameter :: me_program = 'program lister'  ! self-identification

        C ( 1 ) = A

        stop "successful completion for " // me_program // "."  ! string must reduce to constant expression at compilation

    100 format ( /, g0, ':', 1000g0 )
    110 format ( 'B (', I3,' ) = ', 2I2 )
    120 format ( 'C (', I3,' ) = ', 3I2 )
    130 format ( 'D (', I3,' ) = ', 9I2 )

end program lister

! gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 runner.f95