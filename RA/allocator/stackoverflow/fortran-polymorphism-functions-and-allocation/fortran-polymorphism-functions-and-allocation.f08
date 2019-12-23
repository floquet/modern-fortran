! https://stackoverflow.com/questions/29770173/fortran-polymorphism-functions-and-allocation

   module my_module

        implicit none

        type :: my_type
            real :: data
        contains
            procedure              :: sub_copy
            procedure              :: fun_copy_ptr
            procedure              :: fun_copy_alloc
            procedure, pass (this) :: my_assign
            generic                :: assignment(=) => my_assign
        end type my_type

        type, extends(my_type) :: my_derived_type
        end type my_derived_type

    contains

        subroutine sub_copy(this, new)
            class(my_type), intent (in)               :: this
            class(my_type), allocatable, intent (out) :: new

            allocate(new, source=this)
            new%data = new%data + 1

        end subroutine sub_copy

        function fun_copy_alloc(this) result (new)
            class(my_type), intent(in)  :: this
            class(my_type), allocatable :: new

            allocate(new, source=this)
            new%data = new%data + 1.0

        end function fun_copy_alloc

        function fun_copy_ptr(this) result (new)
            class(my_type), intent(in) :: this
            class(my_type), pointer    :: new

            allocate(new, source=this)
            new%data = new%data + 1.0

        end function fun_copy_ptr

        subroutine my_assign(new, this)
            class(my_type), intent(in)               :: this
            class(my_type), allocatable, intent(out) :: new

            allocate(new, source=this)

        end subroutine

    end module my_module

    program my_prog

        use my_module, only: &
            my_type, &
            my_derived_type

        implicit none
        type(my_derived_type)       :: x
        class(my_type), allocatable :: y
        class(my_type), pointer     :: y_ptr => null()

        x%data = 1.0

        ! Case 1
        call x%sub_copy(y)
        print *, y%data
        deallocate(y)

        ! Case 2
        y_ptr => x%fun_copy_ptr()
        print *, y_ptr%data
        deallocate(y_ptr)

        ! Case 3
        allocate( y, source=x%fun_copy_alloc() )
        print *, y%data
        deallocate(y)

    end program my_prog
