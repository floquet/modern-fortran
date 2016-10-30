%-------- DeScRiPtIon ------------------------------------
% This file could be used to generate bathmetry data file for the conical 
% island benchmark test. The list below describes the inputs needed

% dx= gridsize;
% lx  =   length of the tank
% ly  =   width of the tank
% x0  =   location of the center of the cone on the x-axis
% y0  =   location of the center of the cone on the y-axis
% r1  =   radius of the bottom of the cone;
% depth0  =   tank depth;
% hcone   =   cone heght;
% slope   =   cone slope;



clear all
%------------ Input --------------------------------------
dx=0.05;
lx=35.05;
ly=30.05;
x0=12.96;
y0=13.30;
r1=3.6;
depth0=0.32;
hcone=0.625;
slope=0.25;
%---------------------------------------------------------


%----------- Generate Bathymetry -------------------------
x=[0:dx:lx]';
y=[0:dx:ly]';
nx=length(x);
ny=length(y);
h=zeros(ny,ny);
r=zeros(ny,ny);

for i=1:nx
    for j=1:ny
        r(i,j)=sqrt((x(i)-x0)^2+(y(j)-y0)^2);
    end
end

for i=1:nx
    for j=1:ny
        if r(i,j)>r1
            h(i,j)=depth0;
        else
            h(i,j)=(r(i,j)-r1)*slope+depth0;
        end
        
         if h(i,j)<(depth0-hcone)
             h(i,j)=depth0-hcone;
         end
    end
end

pcolor(h);
shading flat
colorbar

dlmwrite('dep.out', h,'delimiter', '\t');