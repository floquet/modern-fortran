! http://www.nag.com/lapack-ex/examples/source/dgesvd-ex.f

!     DGESVD Example Program Text
!     NAG Copyright 2005.
!     .. Parameters ..
        integer :: io_status, io_unit
        character ( len = 512 ) :: io_msg = ""
        character ( len = * ), parameter :: myProgram = "nag_DGESVD", myFile = "dgesvd-ex.d"

      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NB, NMAX
      PARAMETER        (MMAX=10,NB=64,NMAX=8)
      INTEGER          LDA, LDVT, LWORK
      PARAMETER        (LDA=MMAX,LDVT=NMAX,LWORK = MMAX + 4 * NMAX + NB * ( MMAX + NMAX ) )
!     .. Local Scalars ..
      DOUBLE PRECISION EPS, SERRBD
      INTEGER          I, IFAIL, INFO, J, LWKOPT, M, N
!     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DUMMY(1,1), RCONDU(NMAX), RCONDV(NMAX), S(NMAX), UERRBD(NMAX), VERRBD(NMAX), VT(LDVT,NMAX), &
                       WORK(LWORK)
!     .. External Functions ..
      DOUBLE PRECISION DLAMCH
      EXTERNAL         DLAMCH
!     .. External Subroutines ..
      EXTERNAL         DDISNA, DGESVD!, X04CAF
!     .. Executable Statements ..
      WRITE (NOUT,*) 'DGESVD Example Program Results'
      WRITE (NOUT,*)
!     Skip heading in data file
        write ( * , * ) 'Attempting to open ', myFile
        open ( newunit = io_unit, file = myFile, &
        action = 'READ', status = 'OLD', &
        iostat = io_status, iomsg = io_msg )
        if ( io_status /= 0 ) then
            write ( * , 100 ) 'Open', io_status, trim ( io_msg )
            stop 'Unsuccessful open for write for file ' // myFile // ' in ' // myProgram // '.'
        end if

        write ( * , * ) 'Attempting to read ', myFile
      READ (io_unit,*)
      READ (io_unit,*) M, N

      IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
!
!        Read the m by n matrix A from data file
!
         READ (io_unit,*) ((A(I,J),J=1,N),I=1,M)
!
!        Compute the singular values and left and right singular vectors
!        of A (A = U*S*(V**T), m.ge.n)
!
         CALL DGESVD('Overwrite A by U','Singular vectors (V)',M,N,A,LDA,S,DUMMY,1,VT,LDVT,WORK,LWORK,INFO)
         LWKOPT = WORK(1)
!
         IF (INFO.EQ.0) THEN
!
!           Print solution
!
            WRITE (NOUT,*) 'Singular values'
            WRITE (NOUT,99999) (S(J),J=1,N)
!
            IFAIL = 0
!            CALL X04CAF('General',' ',M,N,A,LDA,'Left singular vectors (first n columns of U)',IFAIL)
            WRITE (NOUT,*)
!            CALL X04CAF('General',' ',N,N,VT,LDVT,'Right singular vectors by row (V**T)',IFAIL)
!
!           Get the machine precision, EPS and compute the approximate
!           error bound for the computed singular values.  Note that for
!           the 2-norm, S(1) = norm(A)
!
            EPS = DLAMCH('Eps')
            SERRBD = EPS * S(1)
!
!           Call DDISNA (F08FLF) to estimate reciprocal condition
!           numbers for the singular vectors
!
            CALL DDISNA('Left',M,N,S,RCONDU,INFO)
            CALL DDISNA('Right',M,N,S,RCONDV,INFO)
!
!           Compute the error estimates for the singular vectors
!
            DO 20 I = 1, N
               UERRBD(I) = SERRBD/RCONDU(I)
               VERRBD(I) = SERRBD/RCONDV(I)
   20       CONTINUE
!
!           Print the approximate error bounds for the singular values
!           and vectors
!
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Error estimate for the singular values'
            WRITE (NOUT,99998) SERRBD
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Error estimates for the left singular vectors'
            WRITE (NOUT,99998) (UERRBD(I),I=1,N)
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Error estimates for the right singular vectors'
            WRITE (NOUT,99998) (VERRBD(I),I=1,N)
         ELSE
            WRITE (NOUT,99997) 'Failure in DGESVD. INFO =', INFO
         END IF

!        Print workspace information

         IF (LWORK.LT.LWKOPT) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99996) 'Optimum workspace required = ', LWKOPT, 'Workspace provided         = ', LWORK
         END IF
      ELSE
         WRITE (NOUT,*) 'MMAX and/or NMAX too small'
      END IF
      STOP

      100  format ( g0, ' error:', /, 'iostat = ', g0, /, 'iomsg = ', g0, '.' )
      110  format ( 'Execution continues...' )

99999 FORMAT (3X,(8F8.4))
99998 FORMAT (4X,1P,6E11.1)
99997 FORMAT (1X,A,I4)
99996 FORMAT (1X,A,I5,/1X,A,I5)
      END
