README

1) remove flat -DCARTESION in Makefile
   >make clean
   >make

2) dx and dy conversion
  based on code in init.F
     Dphi_r=Dphi*pi/180.0_SP
     Dtheta_r=Dtheta*pi/180.0_SP
     Dx(I,J) = R_earth*Dphi_r*COS(Lat_theta(I,J))
     Dy(I,J) = R_earth*Dtheta_r
     where R_earth = 6371000.0_SP

! example matlab script for conversion
     dx=20;
     dy=100;
     theta=0.0;
     R=6371000.0;
     dphi=dx/cos(theta)/R*180.0/pi
     dtheta=dy/R*180.0/pi 

3) Synolakis (2007) formula
  matlab code (example: a=height,h=depth,cotbeta=cot(beta))
  a=10;
  h=1000;
  cotbeta=10;
  Rts=2.831*sqrt(cotbeta)*(a/h)^(5/4)*h

Results
1) h=1000m, slope=1/10, Dphi=0.00017986 (~20m),Dtheta = 0.00089932(100m)
   a=10m, rr=29.76, RR=28.31, Error=5.12%
   a=20m, rr=69.18, RR=67.33, Error=2.75%

2) h=100m, slope=1/3, Dphi=0.000017986 (~2m),Dtheta = 0.000089932(10m)
   a=5m rr=12.98, RR=11.59, Error=11.99%
   a=2m rr=4.61, RR=3.69, Error=24.93%
   a=1m rr=2.39, RR=1.55, Error=54.2%

3) h=100m, slope=1/20, Dphi=0.000017986 (~2m),Dtheta = 0.000089932(10m)
   a=1m rr=4.11,  RR=4.00, Error=2.75% (dx=2m)
   a=2m, rr=9.24, RR=9.52, Error=2.9% (dx=2m)
   a=5m, rr=24.85, RR=29.93, Error=16.97% (dx=2m)

4) h=100m, slope=1/10, Dphi=0.000017986 (~2m),Dtheta = 0.000089932(10m)
   a=1m rr=3.16,  RR=2.83, Error=11.6% (dx=2m)
   a=2m, rr=6.97, RR=6.73,Error=3.57%  (dx=2m)
   a=5m, rr=20.53, RR=21.17, Error=3.02% 

5) h=10m, slope=1/20, Dphi=0.0.0000022483(~0.25m),Dtheta = 0.000089932(10m)
   a=0.5m, rr=2.78, RR=2.99, Error=7.02% (dx=0.25m)
   a=0.2m, rr=0.94, RR=0.952, Error=1.26% (dx=0.25m)
   a=0.1m, rr=0.386, RR=0.400,Error=3.50% (dx=0.25m)

6) h=10m, slope=1/10, Dphi=0.0000022483(~0.25m),Dtheta = 0.000089932(10m)
   a=0.1m, rr=0.286, RR=0.283, Error=1.06%
   a=0.2m, rr=0.662,  RR=0.673, Error=1.63%
   a=0.5m, rr=2.172, RR=2.116, Error=2.65%

7) h=10m, slope=1/3, Dphi=0.0000022483(~0.25m),Dtheta = 0.000089932(10m)
   a=0.5m, rr=1.218, RR=1.159, Error=5.09%
   a=0.2m, rr=0.447, RR=0.3688, Error=21.20%
   a=0.1m, rr=0.212, RR=0.1551, Error=36.68%

8) h=1m, slope=1/20, Dphi=0.00000022483(~0.025m),Dtheta = 0.000089932(10m)
   a=0.005m, rr=0.0169, RR=0.0168, Error=0.6%
   a=0.01m, rr=0.0385, RR=0.04, Error=3.75%
   a=0.02m, rr=0.087, RR=0.0952, Error=8.61%
   a=0.05m, rr=0.220, RR=0.299, Error=26.42% (kh=0.80)

9) h=1m, slope=1/10, Dphi=0.00000022483(~0.025m),Dtheta = 0.000089932(10m)
   a=0.05m, rr=0.1892, RR=0.211, Error=10.33%
   a=0.02m, rr=0.0661, RR=0.067, Error=1.34%
   a=0.01m, rr=0.0286, RR=0.0283, Error=1.06%



10) h=1m, slope=1/3, Dphi=0.00000022483(~0.025m),Dtheta = 0.000089932(10m)
   a=0.20m, rr=0.584, RR=0.6558, Error=10.94%
   a=0.10m, rr=0.270, RR=0.2757, Error=2.07%
   a=0.05m, rr=0.122, RR=0.1159, Error=5.26%
   a=0.02m, rr=0.0447, RR=0.0369, Error=21.14%
   a=0.01m, rr=0.0212, RR=0.0155, Error=36.77%

extra tests for convergence

11) h=10m, slope=1/10, Dphi=0.0000022483(~0.25m),Dtheta = 0.000089932(10m) a=0.2
dx=2.0, rr=0.539
dx=1.0m, rr=0.605
dx=0.5m, rr=0.6412
dx=0.4, rr=0.6483
dx=0.3, rr=0.6585
dx=0.25m, rr=0.662
dx=0.2m, rr=0.667
dx=0.125m, rr=0.6716




  
   







