! http://fortranwiki.org/fortran/show/Submodules

program wiki

    use points
    implicit none

    type ( point ) :: point1, point2
    real :: length

        point1 % x = 3.0
        point1 % y = 4.0

        point2 % x = 7.0
        point2 % y = 7.0

        length = point_dist ( point1, point2 )

        print *, '(5) distance between points = ', length

end program wiki
