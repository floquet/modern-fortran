! https://stackoverflow.com/questions/19485666/fortan-final-routine-calls-itself-before-variable-goes-out-of-scope
  module my_type_module
    implicit none

    type my_type
      contains
        final :: destructor
    end type my_type

    interface my_type
      ! Constructor declaration
      module procedure my_type_constructor
    end interface my_type

    contains
      function my_type_constructor()
        type(my_type) :: my_type_constructor

        print *, 'In constructor address is: ', loc(my_type_constructor)

      end function my_type_constructor

      subroutine destructor(this)
        type(my_type) :: this
        print *, 'Destructor of my_type object with address: ', loc(this)
      end subroutine destructor

  end module my_type_module

  program trial

    use my_type_module
    implicit none

    type(my_type) :: argument

    print *, 'Trial program starts'
    print *, 'Initial address is', loc(argument)

    argument = my_type_constructor()

    print *, 'Address afer constructor is called is', loc(argument)

    print *, 'doing some work...'
    print *, 'finishing program...'

    print *, 'Final address is', loc(argument)

 end program trial
 
!  dan-topas-pro-2:final rditldmt$ date
!  Fri Oct 16 09:36:04 CDT 2015
!  dan-topas-pro-2:final rditldmt$ pwd
!  /Users/rditldmt/Box Sync/fortran/demos/stackoverflow/final
!  dan-topas-pro-2:final rditldmt$ gfortran  -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5 final.f08 -o final
!  final.f08:31:8:
!
!       use my_type_module
!          1
!  Warning: Only array FINAL procedures declared for derived type ‘my_type’ defined at (1), suggest also scalar one [-Wsurprising]
!  dan-topas-pro-2:final rditldmt$ ./final
!   Trial program starts
!   Initial address is      140734687451688
!   In constructor address is:       140734687451176
!   Address afer constructor is called is      140734687451688
!   doing some work...
!   finishing program...
