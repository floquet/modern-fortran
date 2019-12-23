! 3456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32

! 2015 10 04

module mLattice

    use mPrecisionDefinitions, only : ip, rp, zero, one
    use mParameters

    implicit none

    type               :: lattice
        integer ( ip ) :: link ( 1 : size, 1 : size, 1 : size, 1 : size, 1 : numDim )
        integer ( ip ) :: pos ( 1 : numDim )
    contains
        private

        procedure, public :: getlink   => getlink_fcn

        procedure, public :: update    => update_sub
        procedure, public :: moveup    => moveup_sub
        procedure, public :: movedown  => movedown_sub
        procedure, public :: coldstart => coldstart_sub
    end type lattice

    private :: getlink_fcn
 
    private :: update_sub
    private :: moveup_sub
    private :: movedown_sub
    private :: coldstart_sub

contains

!   =================================================================================================                     getlink

    function getlink_fcn ( me, dir ) result ( myValue )

        class ( lattice ), target     :: me
        integer ( ip ), intent ( in ) :: dir
        integer ( ip )                :: myValue

            myValue = me % link ( me % pos ( 1 ), me % pos ( 2 ), me % pos ( 3 ), me % pos ( 4 ), dir )

    end function getlink_fcn

!   =================================================================================================                     coldstart

    subroutine coldstart_sub ( me, value )

        class ( lattice ), target     :: me
        integer ( ip ), intent ( in ) :: value

            me % link ( : , : , : , : , : ) = value

    end subroutine coldstart_sub

!   =================================================================================================                        moveup

    subroutine moveup_sub ( me, d )

        class ( lattice ), target     :: me
        integer ( ip ), intent ( in ) :: d  ! dimension

            me % pos ( d ) = me % pos ( d ) + me % pos ( d )
            if ( me % pos ( d ) >= size ) me % pos ( d ) = me % pos ( d ) - size

    end subroutine moveup_sub

!   =================================================================================================                       movedown

    subroutine movedown_sub ( me, d )

        class ( lattice ), target     :: me
        integer ( ip ), intent ( in ) :: d  ! dimension

            me % pos ( d ) = me % pos ( d ) - me % pos ( d )
            if ( me % pos ( d ) < 1 ) me % pos ( d ) = me % pos ( d ) + size

    end subroutine movedown_sub

!   =================================================================================================                         update

    subroutine update_sub ( me, beta, action )

        class ( lattice ), target   :: me
        real ( rp ), intent ( in  ) :: beta
        real ( rp ), intent ( out ) :: action

        real ( rp )                 :: bplus = zero, bminus = zero
        integer ( ip )              :: staple = 0, staplesum = 0
        integer ( ip )              :: kx = 0, ky = 0, kz = 0, kt = 0, kd = 0, kdp = 0

            action = zero

            do kx = 1, size
                do ky = 1, size
                    do kz = 1, size
                        do kt = 1, size
                            do kd = 1, numDim
                                staplesum = 0
                                do kdp = 1, numDim
                                    if ( kd == kdp ) cycle
                                    me % pos = [ kx, ky, kz, kt ]
                                    call me % movedown ( kdp )
                                    !staple = me % getlink ( kdp )
                                    staple = me % link ( me % pos ( 1 ), me % pos ( 2 ), me % pos ( 3 ), me % pos ( 4 ), kdp )
                                end do
                            end do
                        end do
                    end do
                end do
            end do

            action = action / ( size ** numDim * numDim * size )
            action = one - action

    end subroutine update_sub

end module mLattice
