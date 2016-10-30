% a try 
clear all
fdir='/Users/fengyanshi/tmp/';

dep=load('../input/depth.txt');
[ny,mx]=size(dep);
dx=0.1;
dy=0.1;
dt=0.2;
x=[0:mx-1]*0.1;
y=[0:ny-1]*0.1;
x=x-5;
y=y-13;
[X,Y]=meshgrid(x,y);

v=[-0.06:0.005:0.06];

sk=5;
nt=224;
ng=20;
yg(1)=0;xg(1)=7.5;
yg(2)=0;xg(2)=11.5;
yg(3)=0;xg(3)=13.0;
yg(4)=0;xg(4)=15.0;
yg(5)=0;xg(5)=17.0;
yg(6)=0;xg(6)=21.0;

yg(7)=5.;xg(7)=7.5;
yg(8)=5.;xg(8)=11.5;
yg(9)=5.;xg(9)=13.0;
yg(10)=5.;xg(10)=15.0;
yg(11)=5.;xg(11)=17.0;
yg(12)=5.;xg(12)=21.0;

yg(13)=0.;xg(13)=25.;
yg(14)=2.;xg(14)=25.;
yg(15)=5.;xg(15)=25.;
yg(16)=7.;xg(16)=25.;
yg(17)=10.;xg(17)=25.;

yg(18)=0.;xg(18)=13.0;
yg(19)=0.;xg(19)=21.0;
yg(20)=-5.0;xg(20)=21.0;

igjg=input('update igjg, yes =1');
if(igjg==1)
for kg=1:ng
for iy=1:ny-1
if (yg(kg)-y(iy))>=0.&&(yg(kg)-y(iy+1))<0.
if abs(yg(kg)-y(iy)) < abs(yg(kg)-y(iy+1))
jg(kg)=iy;
else
jg(kg)=iy+1;
end
end
end
%jg(kg)

for ix=1:mx-1
if (xg(kg)-x(ix))>=0.&&(xg(kg)-x(ix+1))<0.
if abs(xg(kg)-x(ix)) < abs(xg(kg)-x(ix+1))
ig(kg)=ix;
else
ig(kg)=ix+1;
end
end
end
%ig(kg)
end

save igjg.mat ig jg
else
load igjg.mat;
end

for kt=1:nt
fnum=sprintf('%.4d',kt);
z1=load([fdir 'eta_' fnum]);
u1=load([fdir 'u_' fnum]);
v1=load([fdir 'v_' fnum]);

time1(kt)=(kt-1)*dt;

windowsize=100.;

hh1=0.03+dep;
hh2=0.01+dep;
divx1=diff(hh1.*u1);
divy1=diff(hh1.*v1);
divx2=diff(hh2.*u1);
divy2=diff(hh2.*v1);
w1=-divx1/dx-divy1/dy;
w2=-divx2/dx-divy2/dy;

for kg=1:ng
zg1(kg,kt)=z1(jg(kg),ig(kg));
ug1(kg,kt)=u1(jg(kg),ig(kg));
vg1(kg,kt)=v1(jg(kg),ig(kg));
if(kg==18)
wg1(kg,kt)=w1(jg(kg),ig(kg));
else
wg1(kg,kt)=w2(jg(kg),ig(kg));
end
end

end

FSE_BM2_shi(:,1)=time1(:);
FSE_BM2_shi(:,2)=zg1(1,:);
FSE_BM2_shi(:,3)=zg1(3,:);
FSE_BM2_shi(:,4)=zg1(6,:);
FSE_BM2_shi(:,5)=zg1(7,:);
FSE_BM2_shi(:,6)=zg1(9,:);
FSE_BM2_shi(:,7)=zg1(12,:);
FSE_BM2_shi(:,8)=zg1(13,:);
FSE_BM2_shi(:,9)=zg1(15,:);
FSE_BM2_shi(:,10)=zg1(17,:);

VT_A_BM2_shi(:,1)=time1(:);
VT_A_BM2_shi(:,2)=ug1(18,:);
VT_A_BM2_shi(:,3)=vg1(18,:);
VT_A_BM2_shi(:,4)=wg1(18,:);

VT_B_BM2_shi(:,1)=time1(:);
VT_B_BM2_shi(:,2)=ug1(19,:);
VT_B_BM2_shi(:,3)=vg1(19,:);
VT_B_BM2_shi(:,4)=wg1(19,:);

VT_C_BM2_shi(:,1)=time1(:);
VT_C_BM2_shi(:,2)=ug1(20,:);
VT_C_BM2_shi(:,3)=vg1(20,:);
VT_C_BM2_shi(:,4)=wg1(20,:);

save('FSE_BM2_shi_tvd.txt','-ASCII','FSE_BM2_shi');
save('VT_A_BM2_shi_tvd.txt','-ASCII','VT_A_BM2_shi');
save('VT_B_BM2_shi_tvd.txt','-ASCII','VT_B_BM2_shi');
save('VT_C_BM2_shi_tvd.txt','-ASCII','VT_C_BM2_shi');

break

model.time=time1;
model.zg=zg1;
model.ug=ug1;
model.vg=vg1;
model.wg=wg1;

save 'model_bench2.mat' model


figure
for kp=1:6
subplot(['61' num2str(kp)])
plot(time1(:),zg1(kp,:))
grid
end

figure
for kp=1:6
subplot(['61' num2str(kp)])
plot(time1(:),zg1(kp+6,:))
grid
end

figure
for kp=1:5
subplot(['61' num2str(kp)])
plot(time1(:),zg1(kp+12,:))
grid
end

figure
subplot(611)
plot(time1(:),ug1(18,:))
grid
subplot(612)
plot(time1(:),vg1(18,:))
subplot(613)
plot(time1(:),ug1(19,:))
grid
subplot(614)
plot(time1(:),vg1(19,:))
grid
subplot(615)
plot(time1(:),ug1(20,:))
grid
subplot(616)
plot(time1(:),vg1(20,:))
grid

%axis('square')
%clabel(cf);
%xlabel('x (m)');
%ylabel('y (m)');
%mesh(X(1:4:500,:),Y(1:4:500,:),d1(1:4:500,:))
%view(40,60)
