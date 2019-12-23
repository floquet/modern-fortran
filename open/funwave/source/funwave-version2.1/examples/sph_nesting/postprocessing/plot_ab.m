clear all
fdir1='/Users/fengyanshi/tmp2/';
fdir2='/Users/fengyanshi/tmp4/';

dx1=4;
dx2=2;

ax=[1000 5500 -0.02 0.15]

num='0100'
z1=load([fdir1 'eta_' num]);
z2=load([fdir2 'eta_' num]);

x1=[0:length(z1)-1]*dx1;
x2=[0:length(z2)-1]*dx2;

shift=2004;
clf
subplot(211)
plot(x1,z1,'k')
grid
axis(ax)
text(1200, 0.12, 'Grid A')
ylabel('\eta')
hold on
plot([2000 2000],[-1 1],'r')
text(2050,0.13,'nesting')
text(2050,0.115,'boundary')

subplot(212)
plot(x2+shift,z2,'k')
hold on
grid
axis(ax)
text(1200, 0.12, 'Grid B')
ylabel('\eta')
xlabel('x')
plot([2000 2000],[-1 1],'r')
text(2050,0.11,'t=100s')
text(3050,0.11,'t=200s')
text(4050,0.11,'t=300s')
text(5050,0.11,'t=400s')

num='0200'
z1=load([fdir1 'eta_' num]);
z2=load([fdir2 'eta_' num]);

subplot(211)
plot(x1,z1,'k')
axis(ax)
subplot(212)
plot(x2+shift,z2,'k')
axis(ax)

num='0300'
z1=load([fdir1 'eta_' num]);
z2=load([fdir2 'eta_' num]);
subplot(211)
plot(x1,z1,'k')
axis(ax)
subplot(212)
plot(x2+shift,z2,'k')
axis(ax)

num='0400'
z1=load([fdir1 'eta_' num]);
z2=load([fdir2 'eta_' num]);
subplot(211)
plot(x1,z1,'k')
axis(ax)
subplot(212)
plot(x2+shift,z2,'k')
axis(ax)
