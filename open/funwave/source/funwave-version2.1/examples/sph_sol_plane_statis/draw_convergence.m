dx=[0.125 0.25 0.5 1.0 2.0];
hm=[0.672 0.662 0.6412 0.605 0.539];
hmg=-gradient(hm);
clf
loglog(dx,hmg,'k-o')
%axis([0.1 3.0 0.001 0.2])
grid
xlabel('dx (m)')
ylabel('Difference in setup (m)')
