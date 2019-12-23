!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
program bander
    implicit none

    integer, parameter :: lambda = 6, rows = 20
    integer            :: krows, top, bot
    integer            :: base ( 1 : lambda ) = [ 12, -1, -1, -1, -1, -1 ]
    integer            :: whoops ( 1 : 2 * lambda - 1 ) = [ -1, -1, -1, -1, -1, 12, -1, -1, -1, -1, -1 ]
!    integer            :: column ( 1 : rows ) = 0
    integer            :: A ( 1 : rows, 1 : rows ) = 0


        write ( *, 100 ) 'base', base
        write ( *, 100 ) 'shifted', cshift ( base, 3 )

        do k = 1, krows
            a = krows - lambda
            b = krows + lambda
            [ top, bot ] = [ a, b ]

            if ( top < 1 ) then
                top = 1
            end if

            if ( bot > nrows ) bot = nrows
            A ( krows, top : bot ) = base ( a : b )
        end do

  100   format ( g0, ' = ', 50( 2X, I4 ) )

end program bander