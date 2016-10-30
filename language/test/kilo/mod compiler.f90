module compiler

  use iso_fortran_env, only : compiler_options, compiler_version
  implicit NONE

  private  compiler_options, compiler_version

  ! dimensional parameters
  character ( * ), parameter :: compiled_by   = compiler_version ( )
  character ( * ), parameter :: compiled_with = compiler_options  ( )

end module compiler