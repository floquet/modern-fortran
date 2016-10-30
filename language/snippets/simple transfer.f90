!  Fortran 95/2003 Explained
!  p. 171

!  store a character string as an integer
!  1. define word_in, string to be stored
!  2. cast word_in as an integer, place this value in store
!  3. cast the integer as word_out, the output string
program main

   implicit none

   integer               :: store
   character ( len = 4 ) :: word_in, word_out

!   set storage word
    word_in = "burp"
    write  ( *, 100 ) "input", word_in, store
100 format ( A, ' word = ', g0, '; store = ', g0 )

!   store the word 'burp' as an integer
    store = transfer ( word_in, store )

    word_out = transfer ( store, word_out )
    write  ( *, 100 ) "output", word_out, store

!   store "fart" as an integer, recover as string of length 8
    write  ( *, * ) "one line: ", transfer ( transfer ( "fart", 1 ), "AAAAAAAA" ), "."

end program main