!
!	namelist.f90
!	
!
!	Created by Daniel M. Topa on 6/5/13.
!

program name_list_example

!     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++     DECLARATIONS

      implicit none
      
      integer, parameter    :: sint = selected_int_kind   (  8 )
      integer, parameter    :: lint = selected_int_kind   ( 16 )
      integer, parameter    :: wp   = selected_real_kind  ( 15, 307 )
      
      integer ( sint ), parameter :: nml_unit = 4
!     define parameters inside the namelist
      real ( wp )           :: alpha, beta, df
      integer ( lint )      :: nsweeps
      character (len = 20 ) :: temp, infile, outfile, datafile
      
      
      namelist / simulation_parameters / alpha, beta, df, nsweeps, &
                                         temp, infile, outfile, datafile
                                         
!     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++  OUTPUT NAMELIST

!     create the namelist
      alpha    = 0.5_wp
      beta     = 1.0_wp
      df       = 0.5_wp
      
      nsweeps  = 1000
      
      temp     = 'cold'
      infile   = 'blank'
      outfile  = 'in.601'
      datafile = 'out.601'

!     write the template for the namelist
      open  ( nml_unit, file = 'namelist_template.nml', delim = 'apostrophe' )
      
      write ( unit = nml_unit, nml = simulation_parameters )
      
      close ( nml_unit )
                                         
!     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++      READ UPDATE

 
!     read the updated parameters
      open  ( nml_unit, file = 'namelist_update.nml', delim = 'apostrophe' )
      
      read  ( unit = nml_unit, nml = simulation_parameters )
      
      close ( nml_unit )
      
      write ( *, * ) 'namelist has been updated with new nsweeps'
      
!     only the number of sweeps has changed in the parameter list
      write ( *, * )
      write ( *, * ) 'alpha     = ', alpha
      write ( *, * ) 'beta      = ', beta
      write ( *, * ) 'df        = ', df

      write ( *, * )
      write ( *, * ) 'nsweeps   = ', nsweeps  ! updated with new value
      
      write ( *, * )
      write ( *, * ) 'temp      = ', temp
      write ( *, * ) 'infile    = ', infile
      write ( *, * ) 'outfile   = ', outfile
      write ( *, * ) 'datafile  = ', datafile
     


end program name_list_example