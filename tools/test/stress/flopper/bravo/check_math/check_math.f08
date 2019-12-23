program check_math

    use, intrinsic :: iso_fortran_env,  only : INT64, REAL64

    integer,        parameter :: ip = INT64, rp = REAL64
    integer ( ip ), parameter :: n = 20, iterations = 10**8

    real ( rp ) :: times ( 1 : n ) = 0.0_rp, times_sq ( 1 : n ) = 0.0_rp
    real ( rp ) :: nu    ( 1 : n ) = 0.0_rp, nu_sq    ( 1 : n ) = 0.0_rp

    real ( rp ) :: nu_max, nu_min
    real ( rp ) :: nu_ave = 0.0_rp, nu_var = 0.0_rp, nu_sq_ave = 0.0_rp

    integer :: nu_max_loc = 0_ip, nu_min_loc = 0_ip

        times = [ 7.7510990000000000_rp, 7.7418650000000007_rp, 7.7543970000000009_rp, 7.7952160000000035_rp, &
                  7.7784429999999993_rp, 7.7491459999999961_rp, 7.7844619999999978_rp, 7.7545539999999988_rp, &
                  7.7650009999999909_rp, 7.7730239999999924_rp, 7.7865299999999991_rp, 7.7600229999999897_rp, &
                  7.7601760000000013_rp, 7.7432219999999887_rp, 7.7534559999999999_rp, 7.7542289999999952_rp, &
                  7.7476969999999881_rp, 7.7502379999999960_rp, 7.7647859999999866_rp, 7.7592160000000092_rp  ]

        times_sq ( : ) = times ( : ) * times ( : )

        nu ( : )    = real ( iterations, rp ) / times ( : )
        nu_sq ( : ) = nu ( : ) * nu ( : )

        write ( *, 100 ) 'times',    times ( : )
        write ( *, 100 ) 'times_sq', times_sq ( : )

        write ( *, 100 ) 'nu',    nu ( : )
        write ( *, 100 ) 'nu_sq', nu_sq ( : )

        nu_max = maxval ( nu )
        nu_min = minval ( nu )

        nu_max_loc = maxloc ( nu, dim = 1 )
        nu_min_loc = minloc ( nu, dim = 1 )

        write ( *, 200 ) 'nu max', nu_max, nu_max_loc
        write ( *, 200 ) 'nu min', nu_min, nu_min_loc

        ! construct measurements
        nu_ave    = sum ( nu )    / real ( n, rp )
        nu_sq_ave = sum ( nu_sq ) / real ( n, rp )
        write ( *, 900 ) nu_ave, nu_sq_ave, nu_sq_ave - nu_ave ** 2
        nu_var    = sqrt ( nu_sq_ave - nu_ave ** 2 )

        write ( *, 300 ) nu_ave, nu_var

    100 format ( g0, ':', /, 4 ( 4( g0, ', ' ), g0, / ) )
    200 format ( g0, ' = ', g0, ': at index = ', g0 )
    300 format ( 'average frequency = ', g0, ' +- ', g0 )
    900 format ( 'nu_ave = ', g0, '; nu_sq_ave = ', g0, '; root = ', g0 )

end program check_math

! rditldmt@ITL-DTOPA-MP:check_math $ date
! Mon Jun  6 14:23:18 CDT 2016
! rditldmt@ITL-DTOPA-MP:check_math $ pwd
! /Users/rditldmt/hpc/fortran/tools/test/stress/flopper/bravo/check_math
! rditldmt@ITL-DTOPA-MP:check_math $
! rditldmt@ITL-DTOPA-MP:check_math $ gf check_math
! check_math.f08:20:108:
!
!                    7.7476969999999881_rp, 7.7502379999999960_rp, 7.7647859999999866_rp, 7.7592160000000092_rp  ]
!                                                                                                             1
! Warning: Non-significant digits in 'REAL(8)' number at (1), maybe incorrect KIND [-Wconversion-extra]
! rditldmt@ITL-DTOPA-MP:check_math $
! rditldmt@ITL-DTOPA-MP:check_math $ ./check_math
! times:
! 7.7510990000000000, 7.7418650000000007, 7.7543970000000009, 7.7952160000000035, 7.7784429999999993
! 7.7491459999999961, 7.7844619999999978, 7.7545539999999988, 7.7650009999999909, 7.7730239999999924
! 7.7865299999999991, 7.7600229999999897, 7.7601760000000013, 7.7432219999999887, 7.7534559999999999
! 7.7542289999999952, 7.7476969999999881, 7.7502379999999960, 7.7647859999999866, 7.7592160000000092
!
! times_sq:
! 60.079535707801000, 59.936473678225013, 60.130672833609012, 60.765392486656054, 60.504175504248991
! 60.049263729315939, 60.597848629443966, 60.133107738915982, 60.295240530000861, 60.419902104575883
! 60.630049440899988, 60.217956960528838, 60.220331550976020, 59.957486941283825, 60.116079943936001
! 60.128067384440925, 60.026808803808812, 60.066189056643935, 60.291901625795795, 60.205432934656145
!
! nu:
! 12901396.305220718, 12916784.263223395, 12895909.249939097, 12828380.894127879, 12856043.298125345
! 12904647.815385083, 12846102.916296596, 12895648.157198984, 12878298.405885603, 12865005.948778763
! 12842691.160247250, 12886559.743444076, 12886305.671417760, 12914520.596206611, 12897474.364979953
! 12896188.647510935, 12907061.285437485, 12902829.564717891, 12878654.994484095, 12887900.014640639
!
! nu_sq:
! 166446026624362.78, 166843315702655.53, 166304475382664.78, 164567356364825.22, 165277849283273.59
! 166529935239123.00, 165022360136083.91, 166297741394269.56, 165850569831035.66, 165508378062112.97
! 164934716237492.84, 166063422021353.44, 166056873857213.53, 166784842229844.75, 166344844995315.03
! 166311681632189.91, 166592231026039.16, 166483010776158.06, 165859754466950.12, 166097966787374.19
!
! nu max = 12916784.263223395: at index = 2
! nu min = 12828380.894127879: at index = 4
! nu_ave = 12884420.164863409; nu_sq_ave = 166008867602516.91; root = 584617778.06250000
! average frequency = 12884420.164863409 +- 24178.870487731638

! nb: /Users/rditldmt/Dropbox/ nb/fortran/measure/flops/math/frequency 01.nb
! FullForm[\[Mu]]
!1.288442016486341`*^7

! FullForm[var]
! 24178.870487731638`
