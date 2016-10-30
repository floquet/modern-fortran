module mParameters

    use, intrinsic :: iso_fortran_env,  only : stdout => output_unit
    use mPrecisionDefinitions,          only : ip, rp

    implicit none

    ! real constants
    real ( rp ), parameter      :: zero = 0.0_rp
    real ( rp ), parameter      :: one  = 1.0_rp

    real ( rp ), parameter      :: pi = 3.1415926535897932384626433832795028841971693993751_rp

    real ( rp ), parameter      :: deg_to_rad =  0.017453292519943295769236907684886127134428718885417_rp
    real ( rp ), parameter      :: rad_to_deg = 57.295779513082320876798154814105170332405472466564_rp

end module mParameters
