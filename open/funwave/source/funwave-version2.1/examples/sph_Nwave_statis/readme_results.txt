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

3) use input.txt for cartesion 
   but change Dphi and Dtheta in GRID section 

4) Tadepalli and Synolakis formula
  matlab code (example: a=height,h=depth,cotbeta=cot(beta))
  a=10;
  h=1000;
  cotbeta=10;
  Rts=3.86*sqrt(cotbeta)*(a/h)^(5/4)*h

Results
1) h=1000m, slope=1/10, Dphi=0.00017986 (~20m),Dtheta = 0.00089932(100m)
   a=10m, rr=38.78, RR=38.6, Error=0.47%
   a=20m, rr=88.45, RR=91.81, Error=3.66%
2) h=100m, slope=1/10, Dphi=0.000017986 (~2m),Dtheta = 0.000089932(10m)
   a=5m, rr=26.91, RR=28.86, Error=6.76%
   a=2m, rr=8.87,  RR=9.18, Error=3.38%
3) h=100m, slope=1/3, Dphi=0.000017986 (~2m),Dtheta = 0.000089932(10m)
   a=5m rr=18.52, RR=15.8, Error=17.22%
   a=2m rr=5.42, RR=5.02, Error=7.97%
   a=1m rr=2.45, RR=2.11, Error=16.117%
4) h=100m, slope=1/20, Dphi=0.000017986 (~2m),Dtheta = 0.000089932(10m)
   a=1m rr=5.16,  RR=5.4589, Error=5.48%
   a=2m, rr=11.48, RR=12.98,Error=11.56%  (dx=1m)
   a=5m, rr=22.3, RR=40.81, Error=40.81% (kh=0.52)
5) h=10m, slope=1/20, Dphi=0.0.0000022483(~0.25m),Dtheta = 0.000089932(10m)
   a=0.2m, rr=0.95, RR=1.29, Error=26% (dx=0.5m)
   a=0.2m, rr=1.08, RR=1.29, Error=16.3% (dx=0.25m)
   a=0.1m, rr=0.51, RR=0.546,Error=6.59% (dx=0.25m)
6) h=10m, slope=1/10, Dphi=0.0000022483(~0.25m),Dtheta = 0.000089932(10m)
   a=0.1m, rr=0.380, RR=0.386, Error=1.55%
   a=0.2m, rr=0.87,  RR=0.918, Error=5.23%
   a=0.5m, rr=2.37, RR=2.886, Error=17.88%
7) h=10m, slope=1/3, Dphi=0.0000022483(~0.25m),Dtheta = 0.000089932(10m)
   a=0.5m, rr=1.661, RR=1.581, Error=5.06%
   a=0.2m, rr=0.516, RR=0.5028, Error=2.63%
   a=0.1m, rr=0.244, RR=0.2114, Error=15.42%


8) h=1m, slope=1/20, Dphi=0.00000022483(~0.025m),Dtheta = 0.000089932(10m)
   a=0.005m, rr=0.022, RR=0.0230, Error=4.35%
   a=0.01m, rr=0.049, RR=0.04546, Error=7.79%
   a=0.02m, rr=0.099, RR=0.1298, Error=23.72%
   a=0.05m, rr=0.1864, RR=0.4081, Error=54.32% (kh=0.42)


  
   







