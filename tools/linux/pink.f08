! https://stackoverflow.com/questions/6402700/coloured-terminal-output-from-fortran?rq=1
PROGRAM test

    !PRINT*, '//achar(27)//'[95m pink '//achar(27)//'[0m.'
    !CALL EXECUTE_COMMAND_LINE("echo 'A great color is \033[95m pink \033[0m.'")
    write ( * , ' ( "This text is \x1B[30mblack.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[31mred.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[32mgreen.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[33myellow.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[34mblue.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[35mpurple.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[36maqua.\x1B[0m" ) ' )

    write ( * , ' ( "This text is \x1B[90mdark grey.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[91mpeach.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[92mlight green.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[93mlight yellow.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[94mlight blue.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[95mpink.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[96mlight aqua.\x1B[0m" ) ' )
    write ( * , ' ( "This text is \x1B[97mpearl white.\x1B[0m" ) ' )

END PROGRAM test
