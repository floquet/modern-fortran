program hoster
    implicit none
    integer :: status_host = 0
    character ( len = 8 ) :: host = ''
        status_host = hostnm ( host )
        print *, 'host        = ', trim ( host ), '.'
        print *, 'status_host = ', status_host
end program hoster
