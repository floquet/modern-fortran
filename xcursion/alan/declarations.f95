! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

program declare

    implicit none

    real :: x ( 1 : 1000000000 ) = 0.0
    real :: cpu_0, cpu_1

    character ( len = * ), parameter :: myProgram = 'program transporter'  ! self-identification

        call cpu_time ( cpu_0 )
        x = 1.0
        call cpu_time ( cpu_1 )
        write ( *, '( /, "total cpu time used = ", g0, ": seconds", / )' ) cpu_1 - cpu_0

        stop "successful completion for " // myProgram // "." 

end program declare
