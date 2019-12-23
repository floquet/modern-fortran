! https://gcc.gnu.org/onlinedocs/gcc-6.1.0/gfortran/IAND.html#IAND
    PROGRAM test_iand
        INTEGER :: a, b
        DATA a / Z'F' /, b / Z'3' /
            WRITE (*,*) IAND(a, b)
    END PROGRAM
