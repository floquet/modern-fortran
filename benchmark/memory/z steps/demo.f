NTEGER :: CSTAT, ESTAT
CHARACTER(100) :: CMSG
CALL EXECUTE_COMMAND_LINE (“dir > dir.txt”, EXITSTAT=ESTAT, &
           CMDSTAT=CSTAT, CMDMSG=CMSG)
IF (CSTAT > 0) THEN
  PRINT *, “Command execution failed with error “, TRIM(CMSG)
ELSE IF (CSTAT < 0) THEN
  PRINT *, “Command execution not supported”
ELSE
  PRINT *, “Command completed with status “, ESTAT
END IF
END
