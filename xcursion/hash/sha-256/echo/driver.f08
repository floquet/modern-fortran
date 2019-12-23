program driver

    use mSHA,                           only : Ch, Maj, Sigma_0, Sigma_1, mod_safe_add_232
    use mHexConstants,                  only : ip, nBlocks, H0, K
    implicit none

    integer, parameter :: myBlocks = nBlocks

    integer ( ip ), target  :: register ( 1 : myBlocks ) = 0
    integer ( ip ), pointer :: a => null (), b => null (), c => null (), d => null (), &
                               e => null (), f => null (), g => null (), h => null ()
    integer ( ip ) :: j = 0, k = 0

    do k = 1, myBlocks  ! initialize
        register ( : ) = H0 ( : )
        a => register ( 0 )
        b => register ( 1 )
        c => register ( 2 )
        d => register ( 3 )
        e => register ( 4 )
        f => register ( 5 )
        g => register ( 6 )
        h => register ( 7 )
    end do

    register = compress ( a, b, c, d, e, f, g, h )

    stop "program driver..."

end program driver
