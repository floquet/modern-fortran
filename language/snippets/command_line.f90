! http://www.cs.uwm.edu/~cs151/Bacon/Lecture/HTML/ch12s07.html

!-----------------------------------------------------------------------
!   Program description:
!       Compute a power of command line arguments base and exponent
!
!   Arguments:
!       First:  An integer base
!       Second: An integer exponent
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!   Modification history:
!   Date        Name        Modification
!   2011-03-25  Jason Bacon Begin
!-----------------------------------------------------------------------

module constants
    ! Global Constants
    integer, parameter :: MAX_INTEGER_DIGITS = 10
end module constants

! Main program body
program power
    use constants           ! Constants defined above

    ! Disable implicit declarations (i-n rule)
    implicit none

    ! Variable defintions
    integer :: base, exponent
    character(MAX_INTEGER_DIGITS) :: base_string, exponent_string

    ! First command line argument is the base, second is the exponent
    call getarg(1, base_string)
    call getarg(2, exponent_string)

    ! Make sure user provided both base and exponent
    if ( exponent_string == '' ) then
	stop 'Usage: power base exponent'
    endif

    ! Convert strings to integers
    read (base_string, *) base
    read (exponent_string, *) exponent

    ! Compute power
    print *, base, ' ** ', exponent, ' = ', base ** exponent
end program