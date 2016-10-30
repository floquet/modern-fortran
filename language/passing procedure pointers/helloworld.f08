! http://www.tek-tips.com/viewthread.cfm?qid=1698029
MODULE test_m

    IMPLICIT NONE

    TYPE,PUBLIC :: test
        PROCEDURE( test1 ), POINTER :: test1ptr
    END TYPE test

    ABSTRACT INTERFACE
        SUBROUTINE test1( THIS )
            IMPORT
            IMPLICIT NONE
            CLASS( test ), INTENT( IN ) :: THIS
        END SUBROUTINE test1
    END INTERFACE

    PUBLIC :: temp

CONTAINS

    SUBROUTINE temp( THIS )
        IMPLICIT NONE
        CLASS( test ), INTENT( IN ) :: THIS
        WRITE ( *, * ) 'Hello world from the object!'
    END SUBROUTINE temp

END MODULE test_m

PROGRAM helloworld

    USE test_m

    implicit NONE

    TYPE( test ) :: tester

        tester % test1ptr => temp
        CALL tester % test1ptr()

END PROGRAM helloworld

! dan-topas-pro-2:passing procedure pointers rditldmt$ date
! Fri Jan 15 10:03:44 CST 2016
! dan-topas-pro-2:passing procedure pointers rditldmt$ pwd
! /Users/rditldmt/Box Sync/fortran/demos/passing procedure pointers
! dan-topas-pro-2:passing procedure pointers rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 helloworld.f08
! helloworld.f08:22:25:
!
!      SUBROUTINE temp( THIS )
!                          1
! Warning: Unused dummy argument ‘this’ at (1) [-Wunused-dummy-argument]
! dan-topas-pro-2:passing procedure pointers rditldmt$ ./a.out
!  Hello world from the object!
