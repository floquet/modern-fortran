program test_make

    use iso_fortran_env
    use mThisModule

    implicit none

        write ( * , '( "String from This Module = ", g0, '.' )' ) string

        stop "end of program: test_make"

end program test_make
