module mMathematicaOutput

    use mPrecisionDefinitions,  only : rp
    use mAngle,                 only : angle
    use mLSQ,                   only : lsq_fit

    implicit none

    type ( lsq_fit ), parameter :: mm_lines ( 1 : 3 ) = &
                    ! intercept                gap                       slope
         [ lsq_fit (  3.437728688801653_rp,    0.9898939111077476_rp,    0.33761437784118753_rp,     &
                      0.013022885493030309_rp, 0.0032002478858138956_rp, 0.0016540542769529958_rp ), &
           lsq_fit ( -2.07514689460362_rp,     4.974281484994634_rp,     5.168390165912342_rp,       &
                      0.09207779136468879_rp,  0.05243052831571902_rp,   0.05215080716913731_rp ),   &
           lsq_fit ( -7.505322556682401_rp,    1.2321805928043021_rp,   -0.8576183787656415_rp,      &
                      0.04345443966997083_rp,  0.00388468384065434_rp,   0.003788969817198812_rp )   ]

    type ( angle ), parameter :: mm_angles ( 1 : 3 ) = &
         [ angle ( 1.0403195559542127_rp, 0.018007307681694982_rp ),  &
           angle ( 1.0344988653824094,    0.0029318090945663995_rp ), &
           angle ( 1.0413768606247944_rp, 0.018121411355617643_rp )   ]

end module mMathematicaOutput
