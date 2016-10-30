!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module shared_variables

    use precision_definitions, only : is

    implicit NONE

    character ( len = * ), parameter :: stop_msg = 'Fatal error - ending run.'
    character ( len = * ), parameter :: data_msg = 'Data error - ending run.'

    character ( len = 255 )          :: alloc_msg = " "
    integer   ( is )                 :: alloc_status = 0

end module shared_variables