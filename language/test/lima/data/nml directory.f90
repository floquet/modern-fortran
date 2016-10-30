!     open namelist
      file_name_locator = 'namelist locator.nml'
      open  ( unit = io_unit_nml, file = file_name_locator, delim = 'apostrophe', iostat = io_status )
      if ( io_status /= 0 ) then                               ! can't open file
        write ( *, * )
        write ( *, * ) 'unable to open file ', file_name_locator
        write ( *, * ) 'trying to write namelist command_params'
        write ( *, * ) 'iostat  = ',  io_status
        write ( *, * ) 'io unit = ',  io_unit_nml
        stop  'stop on error during file open'                    ! stop program
      end if

!     write namelist
      write ( unit = io_unit_nml, nml = locator, iostat = io_status )
      if ( io_status /= 0 ) then                              ! can't write file
        write ( *, * )
        write ( *, * ) 'unable to write file ', file_name_locator
        write ( *, * ) 'trying to write namelist command_params'
        write ( *, * ) 'iostat  = ',  io_status
        write ( *, * ) 'io unit = ',  io_unit_nml
        stop  'stop on error during file write'                   ! stop program
      end if

      write ( *, * ) 'successful write to ', file_name_locator

!     close file
      close ( io_unit_nml, iostat = io_status )
      if ( io_status /= 0 ) then                              ! can't write file
        write ( *, * )
        write ( *, * ) 'unable to close file ', file_name_locator
        write ( *, * ) 'trying to write namelist command_params'
        write ( *, * ) 'iostat  = ',  io_status
        write ( *, * ) 'io unit = ',  io_unit_nml
        stop  'stop on error during file close'                   ! stop program
      end if