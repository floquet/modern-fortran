! Tomas Mondragon
! Computer Scientist
! USACE ERDC Information Technology Laboratory
! Computational Analysis Branch
!   delivered 06 Feb 2016
MODULE mFileHandling

     use, intrinsic :: iso_fortran_env, only : input_unit, output_unit, error_unit

     IMPLICIT NONE

     INTEGER, PARAMETER                              :: stdout = OUTPUT_UNIT
     INTEGER, PARAMETER                              :: stdin  = INPUT_UNIT
     INTEGER, PARAMETER                              :: stderr = ERROR_UNIT

CONTAINS

    FUNCTION safeopen_readonly(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

            INQUIRE(FILE=filename, EXIST=exists, OPENED=opened)
            IF(.NOT. exists) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: Could not open ",TRIM(filename)," for reading; file doesn't exist."
                STOP
            ELSE IF (opened) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: Refused to open ",TRIM(filename)," for reading; file already opened."
                STOP
            END IF

            OPEN(NEWUNIT=fd, FILE=filename, ACTION='READ', IOSTAT=io_err, IOMSG=io_message)
            IF(io_err .NE. 0) THEN
                WRITE(stderr,'(A)') io_message
                STOP
            END IF
    END FUNCTION safeopen_readonly

    FUNCTION safeopen_readwrite(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

        INQUIRE(FILE=filename, EXIST=exists, OPENED=opened)
        IF(.NOT. exists) THEN
            WRITE(stderr,'(A,A,A)') "Warning: ",TRIM(filename)," doesn't exist; new empty file will be created."
        ELSE IF (opened) THEN
            WRITE(stderr,'(A,A,A)') "ERROR: Refused to open ",filename,"; file already opened."
            STOP
        END IF

        OPEN(NEWUNIT=fd, FILE=filename, ACTION='READWRITE', IOSTAT=io_err, IOMSG=io_message)
        IF(io_err .NE. 0) THEN
            WRITE(stderr,'(A)') io_message
            STOP
        END IF
    END FUNCTION safeopen_readwrite

    FUNCTION safeopen_writenew(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

            INQUIRE(FILE=filename, EXIST=exists, OPENED=opened)
            IF(exists) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: ",TRIM(filename)," already exists; refused to open in case of overwrite."
                STOP
            ELSE IF (opened) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: ",TRIM(filename)," doesn't exist but is already open;", &
                                        "something is seriously wrong with your OS."
                STOP
            END IF

            OPEN(NEWUNIT=fd, FILE=filename, ACTION='WRITE', STATUS='NEW', IOSTAT=io_err, IOMSG=io_message)
            IF(io_err .NE. 0) THEN
                WRITE(stderr,'(A)') io_message
                STOP
            END IF
    END FUNCTION safeopen_writenew

    FUNCTION safeopen_writeappend(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

            INQUIRE(FILE=filename, EXIST=exists, OPENED=opened)
            IF(.NOT.exists) THEN
                WRITE(stderr,'(A,A,A)') "Warning: ",TRIM(filename)," doesn't exist; new empty file will be created."
            ELSE IF (opened) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: ",TRIM(filename)," is already open; refused to open to prevent ", &
                                        "unintentional writing."
                STOP
            END IF

            IF (exists) THEN
                OPEN(NEWUNIT=fd, FILE=filename, STATUS="old", POSITION="append", ACTION="write", IOSTAT=io_err, IOMSG=io_message)
                IF(io_err .NE. 0) THEN
                    WRITE(stderr,'(A)') io_message
                    STOP
                END IF
            ELSE
                OPEN(NEWUNIT=fd, FILE=filename, STATUS="new", ACTION="write", IOSTAT=io_err, IOMSG=io_message)
                IF(io_err .NE. 0) THEN
                    WRITE(stderr,'(A)') io_message
                STOP
                END IF
            END IF
    END FUNCTION safeopen_writeappend

    FUNCTION safeopen_writereplace(filename) RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=*),INTENT(IN)                   :: filename
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err
        LOGICAL                                       :: opened, exists

            INQUIRE(FILE=filename, EXIST=exists, OPENED=opened)
            IF(.NOT.exists) THEN
                WRITE(stderr,'(A,A,A)') "Warning: ",TRIM(filename)," doesn't exist; new empty file will be created."
            ELSE IF (opened) THEN
                WRITE(stderr,'(A,A,A)') "ERROR: ",TRIM(filename)," is already open; refused to open to prevent ", &
                                        "unintentional writing."
                STOP
            END IF

            OPEN(NEWUNIT=fd, FILE=filename, STATUS="REPLACE", ACTION="write", IOSTAT=io_err, IOMSG=io_message)
            IF(io_err .NE. 0) THEN
                WRITE(stderr,'(A)') io_message
                STOP
            END IF
    END FUNCTION safeopen_writereplace

    FUNCTION safeopen_scratchfile() RESULT(fd)
        IMPLICIT NONE
        INTEGER                                       :: fd
        CHARACTER(LEN=256)                            :: io_message = ""
        INTEGER                                       :: io_err

            OPEN(NEWUNIT=fd, STATUS="SCRATCH", ACTION="READWRITE", IOSTAT=io_err, IOMSG=io_message)
            IF(io_err .NE. 0) THEN
                WRITE(stderr,'(A)') io_message
                STOP
            END IF
    END FUNCTION safeopen_scratchfile

    SUBROUTINE close_and_delete(fd)
        IMPLICIT NONE
        INTEGER,INTENT(IN)                            :: fd
        LOGICAL                                       :: opened

        INQUIRE(UNIT=fd, OPENED=opened)
        IF (.NOT.opened) THEN
            WRITE(stderr,'(A,i4,A)') "ERROR: file descriptor unit ",fd," is not open; refused to delete ", &
                                     "to prevent unintentional deletion."
            STOP
        END IF
        CLOSE(UNIT=fd,STATUS='DELETE')
    END SUBROUTINE close_and_delete

    FUNCTION close_and_keep_scratchfile(fd) RESULT(filename)
        IMPLICIT NONE
        INTEGER, INTENT(IN)                           :: fd
        CHARACTER(LEN=256)                              :: filename
        LOGICAL                                       :: named, opened

            INQUIRE(UNIT=fd, NAMED=named, OPENED=opened)
            IF (opened) THEN
                IF (named) THEN
                    INQUIRE(UNIT=fd,NAME=filename)
                    WRITE(stderr,'(A,i6,A,A,A)') "File descriptor unit ",fd," is named ",TRIM(filename),"."
                ELSE
                    WRITE(stderr,'(A,i6,A)') "File descriptor unit ",fd," is unnamed. Naughty computer!"
                    STOP
                END IF
                    CLOSE(UNIT=fd,STATUS="KEEP")
            ELSE
                WRITE(stderr,'(A,i6,A)') "File descriptor unit ",fd," isn't open. Naughty programmer!"
                STOP
            END IF
    END FUNCTION close_and_keep_scratchfile

  ! This is just an informative printer of information about a given IO file descriptor unit
    SUBROUTINE find_IU_info(fd)
        IMPLICIT NONE
        INTEGER,INTENT(IN)                            :: fd
        CHARACTER(LEN=256)                            :: filename, omode, pos
        LOGICAL                                       :: opened, exists, named

            INQUIRE(UNIT=fd, OPENED=opened, EXIST=exists, NAMED=named, ACTION=omode, POSITION=pos)
            IF (opened) THEN
                WRITE(stdout,'(A,i6,A,A,A,A,A)') "File descriptor unit ",fd," is opened", &
                                                 "in mode ",TRIM(omode), "at position ",TRIM(pos),"."
            ELSE
                WRITE(stdout,'(A,i6,A)') "File descriptor unit ",fd," is not open."
            END IF
            IF (exists) THEN
                WRITE(stdout,'(A,i6,A)') "File descriptor unit ",fd," is in the range of values ", &
                                         "allowed by the compiler."
            ELSE
                WRITE(stdout,'(A,i6,A)') "File descriptor unit ",fd," not allowed by the compiler."
            END IF
            IF (named) THEN
                INQUIRE(UNIT=fd,NAME=filename)
                WRITE(stdout,'(A,i6,A,A,A)') "File descriptor unit ",fd," is named ",TRIM(filename),"."
            ELSE
                WRITE(stdout,'(A,i6,A)') "File descriptor unit ",fd," is unnamed."
            END IF
    END SUBROUTINE find_IU_info

END MODULE mFileHandling
