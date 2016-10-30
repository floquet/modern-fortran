! http://stackoverflow.com/questions/31756906/recursive-fortran-function-return-array?rq=1
module fib
    implicit none
contains
    recursive function Fibbo ( l ) result ( x )
        implicit none
        integer, INTENT ( IN )   :: l  ! input determines size of array
        integer, dimension ( l ) :: x

            if ( l == 1 ) then
                x = 1
            else if ( l == 2 ) then
                x ( 1 ) = 1
                x ( 2 ) = 1
            else
                x ( 1 : l - 1 ) = Fibbo ( l - 1 )
                x ( l ) = x ( l - 1 ) + x ( l - 2 )
            end if
        end function Fibbo

end module fib

program test
    use fib,  only : Fibbo
    implicit none
    integer, parameter :: n = 10
        print *, 'first ', n, ' Fibonacci numbers = ', Fibbo( n )
        print *, 'Mathematica: Fibonacci[ Range[ 10 ] ]'
        print *, '{1, 1, 2, 3, 5, 8, 13, 21, 34, 55}'
end program test

! dantopa@Muntz-Szasz.attlocal.net:recursive array function  $ date
! Sun May 29 17:49:03 CDT 2016
! dantopa@Muntz-Szasz.attlocal.net:recursive array function  $ pwd
! /Users/dantopa/Documents/hpc/fortran/language/stackoverflow/recursive array function
! dantopa@Muntz-Szasz.attlocal.net:recursive array function  $
! dantopa@Muntz-Szasz.attlocal.net:recursive array function  $ gfortran $gflags -o fibonacci fibonacci.f08
! dantopa@Muntz-Szasz.attlocal.net:recursive array function  $ ./fibonacci
!  first           10  Fibonacci numbers =            1           1           2           3           5           8          13          21          34          55
!  Mathematica: Fibonacci[ Range[ 10 ] ]
!  {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}
