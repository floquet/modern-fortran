! http://thy.phy.bnl.gov/~creutz/z2/z2.c

! Z_2 lattice gauge simulation
! Michael Creutz <creutz@bnl.gov>
! http://thy.phy.bnl.gov/~creutz/z2.c

include '/Users/rditldmt/SVN wd/lattice/cruetz/trunk/library/data types.f90'
include '/Users/rditldmt/SVN wd/lattice/cruetz/trunk/library/basic parameters.f90'
include '/Users/rditldmt/SVN wd/lattice/cruetz/trunk/library/simulation parameters.f90'
include '/Users/rditldmt/SVN wd/lattice/cruetz/trunk/library/module lattice.f90'

program translate

!  use ISO_FORTRAN_ENV
  use data_types
  use simulation_parameters
  use lattice

  implicit none

  integer ( INT64 )  :: nSteps = 8_is, k
  real    ( REAL64 ) :: beta_max = 1.0D0, beta, dbeta, action = 0.0D0

  interface fcn_update
    real ( REAL64 ) function update ( beta )
    use ISO_FORTRAN_ENV
    real ( REAL64 ), intent ( in ) :: beta
    end function update
  end interface fcn_update

!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
    call srand ( 1 )

    dbeta = beta_max / nSteps

    call coldstart ( )

!   heat up the lattice
    do k = 0, nSteps
      beta = beta_max - dbeta * k
      action = update ( beta )
      write ( *, * ) beta, action
    end do

!   cool down the lattice
    do k = 0, nSteps
      beta = dbeta * k
      action = update ( beta )
      write ( *, * ) beta, action
    end do

  stop ( "end program cruetz" )

end program translate

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      UPDATE  FCN

real ( REAL64 ) function update ( beta )

  use ISO_FORTRAN_ENV
  use lattice

  implicit none

  integer ( INT64 )              :: x ( 0 : num_dim - 1 ), d, dperp
  real ( REAL64 ), intent ( in ) :: beta
  real ( REAL64 )                :: staplesum = 0.0D0, staple = 0.0D0

    update = 0.0D0

!   loop over spactime
    do x( 0 ) = 1, size
      do x( 1 ) = 1, size
        do x( 2 ) = 1, size
          do x( 3 ) = 1, size
!           loop over directions
            do d = 1, num_dim
              staplesum = 0.0D0
              do dperp = 1, num_dim

                if ( dperp == d ) cycle
!                 move around thusly:
!                 dperp        6--5
!                 ^            |  |
!                 |            1--4
!                 |            |  |
!                 -----> d     2--3
!               plaquette 1234
                movedown ( x, dperp )
                staple = link ( x ( 0 ), x ( 1 ), x ( 2 ), x ( 3 ), dperp ) * link ( x ( 0 ), x ( 1 ), x ( 2 ), x ( 3 ), d )
                staple = staple * link ( x ( 0 ), x ( 1 ), x ( 2 ), x ( 3 ), dperp )
                staplesum = staplesum + staple
!               plaquette 1456
                staple = link ( x ( 0 ), x ( 1 ), x ( 2 ), x ( 3 ), dperp )
                staple = staple * link ( x ( 0 ), x ( 1 ), x ( 2 ), x ( 3 ), d )
                staple = staple * link ( x ( 0 ), x ( 1 ), x ( 2 ), x ( 3 ), dperp )
                staplesum = staplesum + staple

              end do ! dperp
            end do ! d

          end do ! x( 3 )
        end do ! x( 2 )
      end do ! x( 1 )
    end do ! x( 0 )

end function update

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   COLDSTART  SUB

subroutine coldstart ( )

  use lattice

  implicit none

    link = 1_is

end subroutine coldstart

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                      MOVEUP  SUB

subroutine moveup ( x, direction )

  use simulation_parameters

  implicit none

  integer ( is ), intent ( in )    :: direction
  integer ( is ), intent ( inout ) :: x ( 0 : dim )

!   enforce periodic boundary conditions
    x ( direction ) = x ( direction ) + 1
    if ( x ( direction ) >= size ) x ( direction ) = x ( direction ) - size

end subroutine moveup

!   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                    MOVEDOWN  SUB

subroutine movedown ( x, direction )

  use simulation_parameters

  implicit none

  integer ( is ), intent ( in )    :: direction
  integer ( is ), intent ( inout ) :: x ( 0 : dim )

!   enforce periodic boundary conditions
    x ( direction ) = x ( direction ) - 1
    if ( x ( direction ) < 0 ) x ( direction ) = x ( direction ) + size

end subroutine movedown
