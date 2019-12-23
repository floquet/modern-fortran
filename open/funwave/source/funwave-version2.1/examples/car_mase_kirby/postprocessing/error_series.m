clear all
%fdir='/Users/fengyanshi/tmp/';
fdir='';

for k=2:12
num=sprintf('%.2d',k);
gage=load([fdir 'sta_00' num]);
z(:,k)=gage(:,2)*100+(k-2)*10;
model(:,k-1)=gage(:,2)*100;
end
time=gage(:,1);
model_time=time;

% data
%'loading standard data'

fnam(:,12)='r2d025.dat';
fnam(:,11)='r2d050.dat';
fnam(:,10)='r2d075.dat';
fnam(:,9)='r2d100.dat';
fnam(:,8)='r2d125.dat';
fnam(:,7)='r2d150.dat';
fnam(:,6)='r2d175.dat';
fnam(:,5)='r2d200.dat';
fnam(:,4)='r2d250.dat';
fnam(:,3)='r2d300.dat';
fnam(:,2)='r2d350.dat';
fnam(:,1)='r2d470.dat';

for k=1:12
num=sprintf('%.2d',k);
gage_m=load(fnam(:,k));
en(:,k)=gage_m;
end

data_time=[0:length(gage_m)-1]*0.05;
for k=2:12
data(:,k-1)=en(:,k);
end

for k=1:11
data_eta=data(:,k);
model_eta=model(:,k);
std_data(k)=std(data_eta);
std_model(k)=std(model_eta);
relative_error=abs(std_data(k)-std_model(k))/std_data(k)*100.
end

error=rmse(std_model,std_data)/max(std_data)*100

