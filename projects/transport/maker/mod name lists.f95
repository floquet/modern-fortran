module mNameLists

    use precision_definitions, only : is, wp, zero, one
    use param_simulation
    implicit none

    real ( wp ) :: origin ( 1 : numDims )
    character ( len = 11 ) :: nl_names ( 1 : 4 )

    namelist / nlRadius /    bound_radial, mfp, origin, nPts
    namelist / nlThickness / thick_radial, mfp, origin, nPts
    namelist / nFiles /      nl_names


end module mNameLists
