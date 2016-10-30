clear all
%fdir='/Users/fengyanshi/tmp/';
fdir='';

for k=1:12
num=sprintf('%.2d',k);
gage=load([fdir 'sta_00' num]);
z(:,k)=gage(:,2)*100+(k-2)*10;
end
time=gage(:,1);

%clf

for k=2:12
plot(time,z(:,k),'k--')
if k==2
hold on
end
end
mea=load('mketa.out');
time_m=[0:length(mea)-1]*0.01+20.;
for k=1:11
zm=mea(:,k)*100+(k-1)*10;
plot(time_m,zm,'k-');
end
grid
axis([20 40 -10 110])
xlabel('Time (sec)')
ylabel('Surface elevation (cm)')
txt(1,1:4)='35.0';
txt(2,1:4)='30.0';
txt(3,1:4)='25.0';
txt(4,1:4)='20.0';
txt(5,1:4)='17.5';
txt(6,1:4)='15.0';
txt(7,1:4)='12.5';
txt(8,1:4)='10.0';
txt(9,1:4)=' 7.5';
txt(10,1:4)=' 5.0';
txt(11,1:4)=' 2.5';

for k=1:11
text(36,5+(k-1)*10,['h=' txt(k,1:4) 'cm'],'FontSize',12)
end


