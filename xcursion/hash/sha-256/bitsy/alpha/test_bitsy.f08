program test_bitsy

    use, intrinsic :: iso_fortran_env,  only : INT32
    use bitsy, only : swap32

    implicit none
    integer, parameter :: ip = INT32

    ! integer(kind=4) :: ipad1, ipad2, ipad3, ipad4, ipad5, ipad6
    ! integer(kind=4) :: shftc4_r2, shftc4_l12
    ! integer(kind=4) :: shft5_r8, shft5_l11
    integer ( ip ) :: abc_bin, a_bin, empty_str_bin, empty_bin
    ! integer(kind=4) :: abc_bin_flip, a_bin_flip, empty_str_bin_flip
    integer ( ip ) :: abc_bin_ref, abc_bin_swap

    ! integer(kind=4) :: cabc_bin, abca_bin, bcab_bin, ca_one_zero
    ! integer(kind=4) :: cabc_bin_flip, abca_bin_flip, bcab_bin_flip, ca_one_zero_flip
    ! integer(kind=4) :: big_endian_464, little_endian_464

    ! data ipad1 / b'00000000000000000000000000000011' /
    ! data ipad2 / b'11111111111111111111111111111111' /
    ! data ipad3 / b'10010000101001110011001110010011' /
    ! data ipad4 / b'11001001101001110011001110010011' /
    ! data ipad5 / b'10000001101001010011000110100001' /
    ! data ipad6 / b'11000000000000000000000000000000' /

    ! data shftc4_r2          / b'11110010011010011100110011100100' /
    ! data shftc4_l12         / b'01110011001110010011110010011010' /
    ! data shft5_r8           / b'00000000100000011010010100110001' /
    ! data shft5_l11          / b'00101001100011010000100000000000' /
    data abc_bin            / b'00000001011000110110001001100001' /
    ! data a_bin              / b'00000000000000000000000101100001' /
    ! data empty_str_bin      / b'00000000000000000000000000000001' /
    ! data empty_bin          / b'00000000000000000000000000000000' /
    ! data abc_bin_flip       / b'01100001011000100110001110000000' /
    ! data empty_str_bin_flip / b'10000000000000000000000000000000' /
    ! data a_bin_flip         / b'01100001100000000000000000000000' /

    ! data abca_bin           / b'01100001011000110110001001100001' /
    ! data bcab_bin           / b'01100010011000010110001101100010' /
    ! data cabc_bin           / b'01100011011000100110000101100011' /
    ! data ca_one_zero        / b'00000000000000010110000101100011' /
    ! data big_endian_464     / b'11010000000000010000000000000000' /
    ! data little_endian_464  / b'00000000000000000000000111010000' /

    data abc_bin_ref        / b'00000001011000110110001001100001' /
    data abc_bin_swap       / b'01100001011000100110001100000001' /

    ! data abca_bin_flip      / b'01100001011000100110001101100001' /
    ! data bcab_bin_flip      / b'01100010011000110110000101100010' /
    ! data cabc_bin_flip      / b'01100011011000010110001001100011' /
    ! data ca_one_zero_flip   / b'01100011011000011000000000000000' /

        ! Test the swap function.
        print *, 'testing test_swap32...'
        print *, 'swap32 ( abc_bin ) = ', swap32 ( abc_bin )
        print *, 'abc_bin_swap       = ', abc_bin_swap
        print *, 'difference         = ', abc_bin_swap - swap32 ( abc_bin )
        print *, 'abc_bin     = ', abc_bin
        print *, 'abc_bin_ref = ', abc_bin_ref
        print *, 'difference  = ', abc_bin_ref - abc_bin

end program test_bitsy
