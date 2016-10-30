module test_m
  implicit none

  type :: test_t
    character(len=10) :: label
  contains
    procedure :: print => print_test
  end type test_t

  type,extends(test_t) :: test2_t
    character(len=10) :: label2
  contains
    procedure :: print => print_test2
  end type test2_t

  abstract interface
    function if_new_test(lbls) result(t)
      import :: test_t
      class(test_t), allocatable :: t
      character(len=*),intent(in) :: lbls(:)
    end function if_new_test

    subroutine if_make_test(t,lbls)
      import :: test_t
      class(test_t), allocatable :: t
      character(len=*),intent(in) :: lbls(:)
    end subroutine if_make_test
  end interface

contains

  subroutine print_test(self)
    class(test_t), intent(in) :: self
    print *, self%label
  end subroutine print_test

  subroutine print_test2(self)
    class(test2_t), intent(in) :: self
    print *, self%label, self%label2
  end subroutine print_test2

  subroutine make_test(t,lbls)
    class(test_t), allocatable :: t
    character(len=*),intent(in) :: lbls(:)
    allocate(test_t::t)
    t%label = lbls(1)
  end subroutine make_test

  subroutine make_test2(t,lbls)
    class(test_t), allocatable :: t
    character(len=*),intent(in) :: lbls(:)
    allocate(test2_t::t)
    select type(t) ! so the compiler knows the actual type
    type is(test2_t)
      t%label  = lbls(1)
      t%label2 = lbls(2)
    class default
      stop 1
    end select
  end subroutine make_test2

end module test_m


program test
   use test_m
   implicit none

   class(test_t), allocatable :: p
   procedure(if_make_test), pointer :: mt

   mt => make_test
   call mt(p, ["foo"])
   call p%print
   deallocate(p)

   mt => make_test2
   call mt(p, ["bar","baz"])
   call p%print
   deallocate(p)

end program test
