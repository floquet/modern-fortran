! https://gcc.gnu.org/onlinedocs/gfortran/MOD.html
program test_mod
        print *, 'mod(17,3)        = ', mod(17,3)
        print *, 'mod(17.5,5.5)    = ', mod(17.5,5.5)
        print *, 'mod(17.5d0,5.5)  = ', mod(17.5d0,5.5)
        print *, 'mod(17.5,5.5d0)  = ', mod(17.5,5.5d0)

        print *, 'mod(-17,3)       = ', mod(-17,3)
        print *, 'mod(-17.5,5.5)   = ', mod(-17.5,5.5)
        print *, 'mod(-17.5d0,5.5) = ', mod(-17.5d0,5.5)
        print *, 'mod(-17.5,5.5d0) = ', mod(-17.5,5.5d0)

        print *, 'mod(17,-3)       = ', mod(17,-3)
        print *, 'mod(17.5,-5.5)   = ', mod(17.5,-5.5)
        print *, 'mod(17.5d0,-5.5) = ', mod(17.5d0,-5.5)
        print *, 'mod(17.5,-5.5d0) = ', mod(17.5,-5.5d0)
end program test_mod

! dantopa@Muntz-Szasz.local:rosetta $ pwd
! /Users/dantopa/Documents/hpc/fortran/xcursion/hash/sha-256/rosetta
! dantopa@Muntz-Szasz.local:rosetta $ date
! Sat Jun 11 14:43:24 CDT 2016
! dantopa@Muntz-Szasz.local:rosetta $ gf test_mod
! test_mod.f08:5:51:
!
!          print *, 'mod(17.5d0,5.5)  = ', mod(17.5d0,5.5)
!                                                    1
! Warning: GNU Extension: Different type kinds at (1)
! test_mod.f08:6:49:
!
!          print *, 'mod(17.5,5.5d0)  = ', mod(17.5,5.5d0)
!                                                  1
! Warning: GNU Extension: Different type kinds at (1)
! test_mod.f08:10:52:
!
!          print *, 'mod(-17.5d0,5.5) = ', mod(-17.5d0,5.5)
!                                                     1
! Warning: GNU Extension: Different type kinds at (1)
! test_mod.f08:11:50:
!
!          print *, 'mod(-17.5,5.5d0) = ', mod(-17.5,5.5d0)
!                                                   1
! Warning: GNU Extension: Different type kinds at (1)
! test_mod.f08:15:51:
!
!          print *, 'mod(17.5d0,-5.5) = ', mod(17.5d0,-5.5)
!                                                    1
! Warning: GNU Extension: Different type kinds at (1)
! test_mod.f08:16:49:
!
!          print *, 'mod(17.5,-5.5d0) = ', mod(17.5,-5.5d0)
!                                                  1
! Warning: GNU Extension: Different type kinds at (1)
! dantopa@Muntz-Szasz.local:rosetta $ ./test_mod
!  mod(17,3)        =            2
!  mod(17.5,5.5)    =    1.00000000
!  mod(17.5d0,5.5)  =    1.0000000000000000
!  mod(17.5,5.5d0)  =    1.0000000000000000
!  mod(-17,3)       =           -2
!  mod(-17.5,5.5)   =   -1.00000000
!  mod(-17.5d0,5.5) =   -1.0000000000000000
!  mod(-17.5,5.5d0) =   -1.0000000000000000
!  mod(17,-3)       =            2
!  mod(17.5,-5.5)   =    1.00000000
!  mod(17.5d0,-5.5) =    1.0000000000000000
!  mod(17.5,-5.5d0) =    1.0000000000000000
