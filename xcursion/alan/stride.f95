! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

program strider

    use iso_fortran_env
    implicit none

    real ( selected_real_kind ( real64 ) ), parameter :: zero = 0.0_real64

    real ( selected_real_kind ( real64 ) )  :: x ( 1 : 100000000 ) = zero
    real ( selected_real_kind ( real64 ) )  :: y = zero

    integer ( selected_int_kind ( int64 ) ) :: k, count, status, stride

    real :: cpu_0, cpu_1

    character ( len = * ), parameter :: myProgram = 'program strider'  ! self-identification
    character ( len = * ), parameter :: c_options = compiler_options( )
    character ( len = * ), parameter :: c_version = compiler_version( )
    character ( len = 255 )          :: host = " ", cmd = " "

        ! queries
        call hostnm      ( host, status )
        call get_command ( cmd )

        ! write identifiers
        write ( *, '( /, "host system       = ", g0    )' ) trim( host )
        write ( *, '(    "compiler version  = ", g0    )' ) c_version
        write ( *, '(    "compiler options  = ", g0    )' ) trim( c_options )
        write ( *, '(    "execution command = ", g0, / )' ) trim( cmd )

        count = size ( x )

        call cpu_time ( cpu_0 )
            x = 1.0_real64
        call cpu_time ( cpu_1 )
        write ( *, '( /, "assignment cpu time used = ", g0, ": seconds for ", g0, " elements" )' ) cpu_1 - cpu_0, count

        call cpu_time ( cpu_0 )
            y = sum ( x )
        call cpu_time ( cpu_1 )
        write ( *, '( /, "intrinsic sum cpu time used = ", g0, ": seconds; y = ", g0 )' ) cpu_1 - cpu_0, y

        call cpu_time ( cpu_0 )
            y = zero
            do k = 1, count
                y = y + x ( k )
            end do
        call cpu_time ( cpu_1 )
        write ( *, '( /, "do loop sum cpu time used = ", g0, ": seconds; y = ", g0 )' ) cpu_1 - cpu_0, y

!       strides  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =
        stride = 2
        call cpu_time ( cpu_0 )
            y = zero
            do k = 1, count, stride
                y = y + x ( k )
            end do
        call cpu_time ( cpu_1 )
        write ( *, '( /, "do loop sum stride ", g0," cpu time used = ", g0, ": seconds; y = ", g0 )' ) cpu_1 - cpu_0, stride, y

        stride = 4
        call cpu_time ( cpu_0 )
            y = zero
            do k = 1, count, stride
                y = y + x ( k )
            end do
        call cpu_time ( cpu_1 )
        write ( *, '( /, "do loop sum stride ", g0," cpu time used = ", g0, ": seconds; y = ", g0 )' ) cpu_1 - cpu_0, stride, y

        stride = 8
        call cpu_time ( cpu_0 )
            y = zero
            do k = 1, count, stride
                y = y + x ( k )
            end do
        call cpu_time ( cpu_1 )
        write ( *, '( /, "do loop sum stride ", g0," cpu time used = ", g0, ": seconds; y = ", g0 )' ) cpu_1 - cpu_0, stride, y

        stride = 100
        call cpu_time ( cpu_0 )
            y = zero
            do k = 1, count, stride
                y = y + x ( k )
            end do
        call cpu_time ( cpu_1 )
        write ( *, '( /, "do loop sum stride ", g0," cpu time used = ", g0, ": seconds; y = ", g0 )' ) cpu_1 - cpu_0, stride, y

        stop "successful completion for " // myProgram // "." 

end program strider

! dan-topas-pro-2:allen rditldmt$ date
! Wed Sep  9 10:10:19 CDT 2015
! dan-topas-pro-2:allen rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/allen
! dan-topas-pro-2:allen rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic  -fmax-errors=5 stride.f95
! dan-topas-pro-2:allen rditldmt$ ./a.out
!
! host system       = dan-topas-pro-2.erdc.dren.mil
! compiler version  = GCC version 5.1.0
! compiler options  = -fPIC -mmacosx-version-min=10.9.4 -mtune=core2 -Og -Wall -Wextra -Wconversion -Wpedantic -fmax-errors=5
! execution command = ./a.out
!
!
! assignment cpu time used = .472473979: seconds for 100000000 elements
!
! intrinsic sum cpu time used = .122478992: seconds; y = 100000000.00000000
!
! do loop sum cpu time used = .344236016: seconds; y = 100000000.00000000
!
! do loop sum stride 2 cpu time used = .178344011: seconds; y = 50000000.000000000
!
! do loop sum stride 4 cpu time used = .975049734E-01: seconds; y = 25000000.000000000
!
! do loop sum stride 8 cpu time used = .731869936E-01: seconds; y = 12500000.000000000
!
! do loop sum stride 100 cpu time used = .102709532E-01: seconds; y = 1000000.0000000000
! STOP successful completion for program stride.