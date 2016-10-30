!  f90_kind.f90
!
!  Module defining the KIND numbers for the NAG Fortran Compiler.
!
!  Copyright 1991-2011 The Numerical Algorithms Group Ltd., Oxford, U.K.
!
!  Malcolm Cohen, Robert Iles, June 1991
!
!  Release 3: 1.1, 98/06/23
!
!  Release 4: 1.1, 99/12/20
!
!  Release 5.1: 1.3, 06/05/25
!
!  Release 5.2: f90_kind.f90 1 2009-12-02 02:54:28Z Malcolm Cohen
!
!  Release 5.3: $Id: f90_kind.f90 1656 2011-10-06 02:18:21Z Malcolm Cohen $
!
Module,Intrinsic :: f90_kind
!
! These constants are obtained from ISO_FORTRAN_ENV in Fortran 2008.
!
    Use Iso_Fortran_Env,Only:int8,int16,int32,int64,real32,real64,real128
!
! Hide the intrinsic functions we use internally.
!
    Intrinsic :: Dim,Kind,Merge,Precision,Selected_Char_Kind,Selected_Real_Kind
    Private :: Dim,Kind,Merge,Precision,Selected_Char_Kind,Selected_Real_Kind
!
! Indicator that the KIND= is not available for this compiler/host
    Integer,Parameter :: not_available = -1  
!
! Real and Complex numbers
!   Single precision
    Integer,Parameter :: single  = Kind(0.0)
!   Double precision
    Integer,Parameter :: double  = Kind(0.0d0)
!   Quadruple precision
    Integer,Parameter :: quad    = Selected_Real_Kind(Precision(1._double)*2-5)
!
! Work out "quad" situation vis-a-vis real128 and real64x2 privately.
! ("quad" is affected by use of the -double option!)
!
    Integer,Private,Parameter :: trueq = Dim(Selected_Real_Kind(33,4900),0)
    Integer,Private,Parameter :: ddq = Dim(Selected_Real_Kind(31,291),0)
!
!   Now ddq is 0 or the ddquad kind (if it exists) or the true quad kind,
!   and trueq is 0 or the true quad kind.  We use both these below.
!
! Real and Complex numbers
!   Two eight byte reals forming a quad
    Integer,Parameter :: real64x2  = Merge(tsource=ddq,fsource=-1, &
                                          mask=(ddq>0 .And. ddq/=trueq))
!
! Logical values
!   Single byte logical
    Integer,Parameter :: byte      = int8
!   Two byte logical
    Integer,Parameter :: twobyte   = int16
!   Four byte logical (Logical uses the same kind numbers as Integer)
    Integer,Parameter :: word      = int32
!   Eight byte logical
    Integer,Parameter :: logical64 = int64
!
! Character kinds
!   Normal single byte character (ASCII sequence)
    Integer,Parameter :: ascii   = Selected_Char_Kind('ASCII')
!   Japanese (JIS X 0213:2004) characters.
    Integer,Parameter :: jis = Selected_Char_Kind('Shift_JIS')
!   Unicode 2-byte characters.
    Integer,Parameter :: ucs2 = Selected_Char_Kind('UCS_2')
!   Full ISO-10646 characters.
    Integer,Parameter :: ucs4 = Selected_Char_Kind('ISO_10646')
End Module f90_kind
