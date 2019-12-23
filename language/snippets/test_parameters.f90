program test_parameters

  real :: a
  
  interface
    subroutine passer ( a )
    real, optional :: a
    end subroutine passer
  end interface
  
  call passer (  )
  
end program test_parameters

subroutine passer ( a )

  real :: A, b
  real, optional, intent ( in ) :: a
  
  if ( .NOT. present ( a ) ) then 
    write ( *, * ) 'true'
  else
    write ( *, * ) 'false'
  end if
  
  if ( .NOT. present ( a ) ) a = 2.
  
  A = 3.

  print *, 'a - ', a 
  
end subroutine passer