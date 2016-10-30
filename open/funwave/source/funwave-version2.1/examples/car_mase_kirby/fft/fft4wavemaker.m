% This is script to make wave components (amplitude and phase) for given time series.  

clear all
% PLEASE INPUT:
data1=load('r2d470.dat'); % one- (ele) or two-column time series (t,ele) 
data=data1(1:14336); % 14*1024
sample_frequency=0.05; 
low_cutoff_freq= 0.2;
high_cutoff_freq = 10;
scale_fac=0.01;
output_file='wavemk_per_amp_pha.txt';

% start to runÉ
[nr,nc]=size(data);
wave = data(:,nc);
t=[0:length(wave)-1]*sample_frequency;
d = fft(wave);
m = length(wave);
M = floor((m+1)/2);
time=sample_frequency*length(wave);
a0 = d(1)/m; 
an = 2*real(d(2:M))/m;
bn = -2*imag(d(2:M))/m;

n = 1:length(an);
x=[0:length(n)-1]*time/(length(n)-1);
%x = 0.05:(time/length(n)):time;

y=zeros(1,length(n));

for i=1:1:length(n)
    for j=1:1:length(n)
        y(i) = y(i)+an(j)*cos(2*pi*j*x(i)/time)+ bn(j)*sin(2*pi*j*x(i)/time);
    end
    y(i)=y(i)+a0;
end

for i=1:length(n)
    cn(i) = sqrt(an(i).^2+bn(i).^2);
    en(i) = atan2(bn(i),an(i));
end

icount=0;
for j=1:length(n)
ff=j/time;
if ff>low_cutoff_freq && ff< high_cutoff_freq
icount=icount+1;
per(icount)=time/j;
cnn(icount)=cn(j);
enn(icount)=en(j);
end
end

z=zeros(1,length(n));
for i=1:1:length(n)
    for j=1:icount
        z(i) = z(i)+cnn(j)*cos(2*pi*x(i)/per(j)-enn(j));
    end
    z(i)=z(i)+a0;
end




%k=[an];
%l=[x; n];

        
clf
plot(x,y,'b','Linewidth',2)
hold on
plot(t,wave,'r')
hold on
plot(x,z,'ko','Linewidth',0.5)
legend('Data','DFT Interpolant','approximated')

% open a file for writing
fid = fopen(output_file, 'w');

cnn=cnn*scale_fac;
%fprintf(fid,'%5d\n',cutoff_number);
for i=1:1:icount
    fprintf(fid,'%12.8f %12.8f %12.8f\n',per(i),cnn(i),enn(i));
end
fclose(fid);




