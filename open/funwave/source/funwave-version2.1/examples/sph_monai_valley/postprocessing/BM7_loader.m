%               NTHMP (National Tsunami Hazard Mitigation Program)
% -------------------------------------------------------------------------
%    This MATLAB script is to compare the measured data with the model
%    results for maximum  runup of a monai valley.(Written by Babak Tehranirad)
%
%    You do not need to upload any files the code will upload it
%    automatically.
%
%    This is a description of what this MATLAB script do:
%
%     Comparing the guages' results
%     
%           - The guage files would get uploaded for two different guage 
%             locations('g7.txt','g9.txt')
%
%           - The first column in the file is the time and the second
%           column is the water surface location at each time
%
%             
%           - The first column in the file is the location and the second
%           column is the water surface location at each time
%
%
% -------------------------------------------------------------------------
clear all
load('A.mat')
data1=load('sta_0002');
data2=load('sta_0003');
data3=load('sta_0004');

scaleT=10;
scaleS=1; % use cm 
lag=0.0;
t=t+lag;
tn1=data1(:,1)/scaleT;
g5n=data1(:,2)/scaleS;
tn2=data2(:,1)/scaleT;
g7n=data2(:,2)/scaleS;
tn3=data3(:,1)/scaleT;
g9n=data3(:,2)/scaleS;

clf
%figure(1)
subplot(3,1,1)
plot(t,g5,'linewidth',1.25)
hold on
plot(tn1,g5n,'--r','linewidth',1.25)
axis([0 24 -2 5])
title('Gauge 5')
ylabel('cm')
grid on



%figure(2)
subplot(3,1,2)
plot(t,g7,'linewidth',1.25)
hold on
plot(tn2,g7n,'--r','linewidth',1.25)
axis([0 24 -2 5])
title('Gauge 7')
ylabel('cm')
grid on


%figure(3)
subplot(3,1,3)
plot(t,g9,'linewidth',1.25)
hold on
plot(tn3,g9n,'--r','linewidth',1.25)
title('Gauge 9')
axis([0 24 -2 5])
xlabel('Time(sec)')
ylabel('cm')
grid on
legend('Measured data','Numerical Simulation','Location','Best');

