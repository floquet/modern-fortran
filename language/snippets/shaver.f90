program shaver
! unformatted write

USE, INTRINSIC             :: ISO_C_BINDING, ONLY: C_INT
INTEGER, PARAMETER         :: unit = 10
CHARACTER ( * ), PARAMETER :: filename = 'singles.r32'
INTEGER ( C_INT )          :: data
!***
OPEN  ( unit = unit, file = filename, ACCESS = 'STREAM', FORM = 'UNFORMATTED')
READ  ( unit ) data
CLOSE ( unit )

write ( *,  "(  a, ' was ', I0, ' bytes' )" ), trim( filename ), data


end program shaver