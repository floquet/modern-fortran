!include 'mod precision definitions.f08'
include 'myModules/mod svd parameters.f08'
include 'myModules/mod matrix writer.f08'

program wtf

    use mSVDparameters
    use mMatrixWriter
    implicit none

        write ( * , * ) 'io_write = ', io_write

        M = 2
        N = 3
        write ( * , * ) 'm + n = ', M + N

        row = 4
        col = 5
        write ( * , * ) 'row + col = ', row + col

end program wtf
