module mRequestPrecision

    use mPrecisionDefinitions,  only : ip, zint
    use mBlocks,                only : announce, nBlock
    use mShared,                only : open_file_output, alert_io, io_status, io_msg
    use mDeclarationPrecision

    implicit none

    contains

        subroutine request_precision ( io_unit, file_out )  !   +   +   +   +   +   +   +   +   +   +   +   +   +  request_precision

            integer ( ip ),        intent ( in )           :: io_unit
            character ( len = * ), intent ( in ), optional :: file_out

            integer ( ip ) :: io_block

            nBlock = nBlock + 1
            if ( present ( file_out ) ) then
                call open_file_output ( file_out, io_block )
                call alert_io ( task = 'OPEN', file_name = file_out, io_handle = io_block )
                call print_request_precision ( io_block )
                close ( io_block, iostat = io_status, iomsg = io_msg )
            else
                call print_request_precision ( io_unit )
            end if

        end subroutine request_precision

        subroutine print_request_precision ( io_unit )  !   +   +   +   +   +   +   +   +   +   +   +   +    print_request_precision

            integer ( ip ) , intent ( in )       :: io_unit
            integer ( ip )                       :: pmax = -huge ( 1_zint ), rmax = -huge ( 1_zint )
            integer ( ip ) , dimension ( 1 )     :: pmaxloc = [ -1 ], rmaxloc = [ -1 ]

                !   Annunciation
                call announce ( "Requests for precision", io_unit )

                !   Results of precision request
                write ( io_unit, '( "selected real kind: -1 signals precision unavailable" )' )
                write ( io_unit, '( "p = requested precision (number of significant digits)" )' )
                write ( io_unit, '( "return value is selected_real_kind: -1 signals precision unavailable" )' )

                write ( io_unit, 100 ) p01,  p02,  p03,  p04,  p05
                write ( io_unit, 110 ) p06,  p07,  p08,  p09,  p10
                write ( io_unit, 120 ) p11,  p12,  p13,  p14,  p15
                write ( io_unit, 130 ) p16,  p17,  p18,  p19,  p20
                write ( io_unit, 140 ) p21,  p22,  p23,  p24,  p25
                write ( io_unit, 150 ) p26,  p27,  p28,  p29,  p30
                write ( io_unit, 160 ) p31,  p32,  p33,  p34,  p35
                write ( io_unit, 170 ) p36,  p37,  p38,  p39,  p40
                write ( io_unit, 180 ) p41,  p42,  p43,  p44,  p45
                write ( io_unit, 190 ) p46,  p47,  p48,  p49,  p50

                pmax    = maxval ( [ p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, &
                         p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, &
                         p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, &
                         p41, p42, p43, p44, p45, p46, p47, p48, p49, p50 ] )
                pmaxloc = maxloc ( [ p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, &
                         p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, &
                         p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40, &
                         p41, p42, p43, p44, p45, p46, p47, p48, p49, p50 ] )

                write ( io_unit, '( "real_kind for maximum precision = ", g0 )' ) pmax
                write ( io_unit, '( "first instance is for p = ", g0, / )' )      pmaxloc

                write ( io_unit, '( "selected integer kind: -1 signals range unavailable" )' )
                write ( io_unit, '( "r = requested range such that 10^(-r) < n < 10^(r)" )' )
                write ( io_unit, '( "return value is selected_int_kind: -1 signals range unavailable" )' )

                write ( io_unit, 300 ) r01, r02, r03, r04, r05
                write ( io_unit, 310 ) r06, r07, r08, r09, r10
                write ( io_unit, 320 ) r11, r12, r13, r14, r15
                write ( io_unit, 330 ) r16, r17, r18, r19, r20
                write ( io_unit, 340 ) r21, r22, r23, r24, r25
                write ( io_unit, 350 ) r26, r27, r28, r29, r30
                write ( io_unit, 360 ) r31, r32, r33, r34, r35
                write ( io_unit, 370 ) r36, r37, r38, r39, r40
                write ( io_unit, 380 ) r41, r42, r43, r44, r45
                write ( io_unit, 390 ) r46, r47, r48, r49, r50
                write ( io_unit, * )

                rmax    = maxval ( [ r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, &
                         r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, &
                         r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, &
                         r41, r42, r43, r44, r45, r46, r47, r48, r49, r50 ] )
                rmaxloc = maxloc ( [ r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, &
                         r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, &
                         r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, &
                         r41, r42, r43, r44, r45, r46, r47, r48, r49, r50 ] )

                write ( io_unit, '( "int_kind for maximum precision = ", g0 )' ) rmax
                write ( io_unit, '( "first instance is for r = ", g0, / )'     ) rmaxloc

            100 format ( "p =  1: ", I3, ",   p =  2: ", I3, ",   p =  3: ", I3, ",   p =  4: ", I3, ",   p =  5: ", I3 )
            110 format ( "p =  6: ", I3, ",   p =  7: ", I3, ",   p =  8: ", I3, ",   p =  9: ", I3, ",   p = 10: ", I3 )
            120 format ( "p = 11: ", I3, ",   p = 12: ", I3, ",   p = 13: ", I3, ",   p = 14: ", I3, ",   p = 15: ", I3 )
            130 format ( "p = 16: ", I3, ",   p = 17: ", I3, ",   p = 18: ", I3, ",   p = 19: ", I3, ",   p = 20: ", I3 )
            140 format ( "p = 21: ", I3, ",   p = 22: ", I3, ",   p = 23: ", I3, ",   p = 24: ", I3, ",   p = 25: ", I3 )
            150 format ( "p = 26: ", I3, ",   p = 27: ", I3, ",   p = 28: ", I3, ",   p = 29: ", I3, ",   p = 30: ", I3 )
            160 format ( "p = 31: ", I3, ",   p = 32: ", I3, ",   p = 33: ", I3, ",   p = 34: ", I3, ",   p = 35: ", I3 )
            170 format ( "p = 36: ", I3, ",   p = 37: ", I3, ",   p = 38: ", I3, ",   p = 39: ", I3, ",   p = 40: ", I3 )
            180 format ( "p = 41: ", I3, ",   p = 42: ", I3, ",   p = 43: ", I3, ",   p = 44: ", I3, ",   p = 45: ", I3 )
            190 format ( "p = 46: ", I3, ",   p = 47: ", I3, ",   p = 48: ", I3, ",   p = 49: ", I3, ",   p = 50: ", I3 )

            300 format ( "r =  1: ", I3, ",   r =  2: ", I3, ",   r =  3: ", I3, ",   r =  4: ", I3, ",   r =  5: ", I3 )
            310 format ( "r =  6: ", I3, ",   r =  7: ", I3, ",   r =  8: ", I3, ",   r =  9: ", I3, ",   r = 10: ", I3 )
            320 format ( "r = 11: ", I3, ",   r = 12: ", I3, ",   r = 13: ", I3, ",   r = 14: ", I3, ",   r = 15: ", I3 )
            330 format ( "r = 16: ", I3, ",   r = 17: ", I3, ",   r = 18: ", I3, ",   r = 19: ", I3, ",   r = 20: ", I3 )
            340 format ( "r = 21: ", I3, ",   r = 22: ", I3, ",   r = 23: ", I3, ",   r = 24: ", I3, ",   r = 25: ", I3 )
            350 format ( "r = 26: ", I3, ",   r = 27: ", I3, ",   r = 28: ", I3, ",   r = 29: ", I3, ",   r = 30: ", I3 )
            360 format ( "r = 31: ", I3, ",   r = 32: ", I3, ",   r = 33: ", I3, ",   r = 34: ", I3, ",   r = 35: ", I3 )
            370 format ( "r = 36: ", I3, ",   r = 37: ", I3, ",   r = 38: ", I3, ",   r = 39: ", I3, ",   r = 40: ", I3 )
            380 format ( "r = 41: ", I3, ",   r = 42: ", I3, ",   r = 43: ", I3, ",   r = 44: ", I3, ",   r = 45: ", I3 )
            390 format ( "r = 46: ", I3, ",   r = 47: ", I3, ",   r = 48: ", I3, ",   r = 49: ", I3, ",   r = 50: ", I3 )

        end subroutine print_request_precision

end module mRequestPrecision
