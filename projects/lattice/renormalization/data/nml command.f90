!     open namelist      
      file_name_command = 'namelist command.nml'
      open  ( unit = io_unit_nml, file = file_name_command, &
                                 delim = 'apostrophe', iostat = io_status )      
      if ( io_status /= 0 ) then                               ! can't open file
        write ( *,  * )
        write ( *,  * ) 'unable to open file ', file_name_command
        write ( *,  * ) 'trying to write namelist command_params'
        write ( *,  * ) 'iostat  = ',  io_status
        write ( *,  * ) 'io unit = ',  io_unit_nml
        stop  'stop on error during file open'                    ! stop program
      end if

!     write namelist      
      write ( unit = io_unit_nml, nml = command_params, iostat = io_status )
      if ( io_status /= 0 ) then                              ! can't write file
        write ( *,  * )
        write ( *,  * ) 'unable to write file ', file_name_command
        write ( *,  * ) 'trying to write namelist command_params'
        write ( *,  * ) 'iostat  = ',  io_status
        write ( *,  * ) 'io unit = ',  io_unit_nml
        stop  'stop on error during file write'                   ! stop program
      end if

      write ( *, * ) 'successful write to ', file_name_command
      
!     close file
      close ( nml_unit, iostat = io_status )
      if ( io_status /= 0 ) then                              ! can't write file
        write ( *,  * )
        write ( *,  * ) 'unable to close file ', file_name_command
        write ( *,  * ) 'trying to write namelist command_params'
        write ( *,  * ) 'iostat  = ',  io_status
        write ( *,  * ) 'io unit = ',  io_unit_nml
        stop  'stop on error during file close'                   ! stop program
      end if