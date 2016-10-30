! Hansen and Tompkins, p. 22
! make all real variables _rp ( working precision )
! then real precision is controlled by one setting
module mParameters

    use, intrinsic :: iso_fortran_env,  only : stdout => output_unit
    use mPrecisionDefinitions,          only : ip, rp
    implicit NONE

    ! real constants
    real ( rp ), parameter      :: zero = 0.0_rp
    real ( rp ), parameter      :: one  = 1.0_rp

    real ( rp ), parameter      :: pi = 3.1415926535897932384626433832795028841971693993751_rp

    real ( rp ), parameter      :: deg_to_rad =  0.017453292519943295769236907684886127134428718885417_rp
    real ( rp ), parameter      :: rad_to_deg = 57.295779513082320876798154814105170332405472466564_rp

    integer ( ip ), parameter   :: megabytes = 1048576

end module mParameters
