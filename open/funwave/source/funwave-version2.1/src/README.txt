README for implementing spherical version with z alpha

+) mod_global.F
define U4xL,U4xR,V4yL,V4yR,
          U4,V4,U1p,V1p

+)io.F
read Beta_ref

+) init.F
assign beta_1

+) init.F
allocate U4xL, U4xR,V4yL, V4yR

+) init.F
allocate U4 and V4

+) main.F
calculate U4 and V4

+) main.F
calculate U1p and V1p

+) fluxes.F
calculate P, Q, F and G in construction and construction_ho

+) main.F
add 1/r_0 tan theta (HV+hV4)

+) main.F
add extra term to U1p and V1p in Cal_dispersion
(this is the correction never done in version 1.0 and 1.1)

+) main.F
add U4 and V4 in SourceX and SourceY, respectively

NOTE: some differences found between version before 09/06 and Jeff's old version
1) main.F mis-used DXg and DYg in without wavemaker
2) init.F the old version used averaging to re-construct depth at centroid
   Now used the same thing for the new version
3) init.F the old version used SLOPE_CTR, now used in the new version
4) main.F defined the extra terms calculation using SPH_EXTRA_DIS
5) Fluxes.F there's still slight difference between 3rd and 2nd order in CONSTRUCT_X and Y
 




