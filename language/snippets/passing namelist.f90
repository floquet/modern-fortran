!
!	passing namelist.f90
!	
!
!	Created by Daniel M. Topa on 6/6/13.
!	Copyright 2013 University of New Mexico. All rights reserved.
!

!     ############################################################       MODULES

module data_types

      implicit NONE

!     parameters
      integer, parameter    :: sint = selected_int_kind   (  8 )
      integer, parameter    :: lint = selected_int_kind   ( 16 )
      integer, parameter    :: sp   = selected_real_kind  (  6,  37 )
      integer, parameter    :: wp   = selected_real_kind  ( 15, 307 )
      
      integer ( sint ), parameter :: nml_unit = 4

!     constants
      real( wp ), parameter :: pi = 3.141592653589793238462643383279502884197_wp
      real( wp ), parameter :: machine_eps = 2.221D-16

end module data_types

!     ############################################################

program passer ! dimensionless variables, careful about energies

      use data_types
      implicit none
      
      real ( wp )            :: a
            
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    BEGIN MAIN

      call get_namelist ( a )
      
      write ( *, * ) 'a from namelist = ', a


end program passer

!     11111111111111111111111111111111111111111111111111111111111111111111     !

subroutine get_namelist ( a )

      use data_types
      implicit none
      
      integer ( sint )       :: io_status
      
      real ( wp )            :: a
      
      character ( len = 64 ) :: file_name
      
      namelist / nlist / a
      
      
      file_name = 'demo.nml'
      
      open  ( nml_unit, file = 'demo.nml', delim = 'apostrophe', &
                        iostat = io_status )      
      if ( io_status /= 0 ) then                               ! can't open file
        write ( *,  * ) ''
        write ( *,  * ) 'unable to open file ', file_name
        write ( *,  * ) 'iostat    = ',  io_status
        write ( *,  * ) 'io unit   = ',  nml_unit
        stop  'stop on error during file open'                    ! stop program
      end if

      read ( unit = nml_unit, nml = nlist, iostat = io_status )
      if ( io_status /= 0 ) then                               ! can't read file
        write ( *,  * ) ''
        write ( *,  * ) 'unable to write file ', file_name
        write ( *,  * ) 'iostat    = ',  io_status
        write ( *,  * ) 'io unit   = ',  nml_unit
        stop  'stop on error during write open'                   ! stop program
      end if
      write ( *, * ) 'successful write to ', file_name
      
      close ( nml_unit )

end subroutine get_namelist
