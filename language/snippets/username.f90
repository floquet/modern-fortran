!     http://gcc.gnu.org/onlinedocs/gfortran/GETLOG.html

PROGRAM TEST_GETLOG

      CHARACTER(32) :: login
      CALL GETLOG(login)
      WRITE(*,*) login
      
END PROGRAM