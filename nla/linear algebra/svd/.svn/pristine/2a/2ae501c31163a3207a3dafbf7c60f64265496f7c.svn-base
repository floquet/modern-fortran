subroutine whoami ( )

  use basic_parameters

! DECLARATIONS
  implicit none

  integer   ( zint )                                 :: status = 0

  character ( * ), parameter                         :: c_options = compiler_options( )
  character ( * ), parameter                         :: c_version = compiler_version( )
  character ( kind = ascii, len = 255 )              :: host = " ", cmd = " "

!   queries
    call hostnm      ( host, status )
    call get_command ( cmd )

!   write identifiers
    write ( *, '( "host system        = ", A )' ) trim( host )
    write ( *, '( "compiler version   = ", A )' ) c_version
    write ( *, '( "compiler options   = ", A )' ) trim( c_options )
    write ( *, '( "invocation command = ", A )' ) trim( cmd )

end subroutine whoami
