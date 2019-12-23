program danny

    implicit none
    real :: a, b
    real :: area, perimeter

        print *, 'Input a:'
        read *, a

        print *, 'Input b:'
        read *, b

        ! rectangle
        area = a * b
        perimeter = 2 * ( a + b )

        print *, 'Rectangle area = ', area
        print *, 'Rectangle perimeter = ', perimeter

        ! right triangle
        area = a * b / 2
        perimeter = a + b + sqrt( a**2 + b**2 )

        print *, 'Right triangle area = ', area
        print *, 'Right triangle perimeter = ', perimeter

end program danny