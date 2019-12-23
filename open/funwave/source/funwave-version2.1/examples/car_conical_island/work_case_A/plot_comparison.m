clear all
data1=load('A.txt');
data6=load('sta_0001');  % gauge 6  use (1) in matlab
data9=load('sta_0002');  % gauge 9  use (2)
data16=load('sta_0008'); % gauge 16 use (3)
data22=load('sta_0013'); % gauge 22 use (4)

time_lag_case_A=24.4;

t=data1(:,1);
measure(:,1)=data1(:,2);
measure(:,2)=data1(:,3);
measure(:,3)=data1(:,4);
measure(:,4)=data1(:,5);

tmodel(:,1)=data6(:,1) + time_lag_case_A;
model(:,1)=data6(:,2);

tmodel(:,2)=data9(:,1) + time_lag_case_A;
model(:,2)=data9(:,2);

tmodel(:,3)=data16(:,1) + time_lag_case_A;
model(:,3)=data16(:,2);

tmodel(:,4)=data22(:,1) + time_lag_case_A;
model(:,4)=data22(:,2);

gauge(1,:)=' 6';
gauge(2,:)=' 9';
gauge(3,:)='16';
gauge(4,:)='22';
ax=[25 41 -0.02 0.04];

clf
for i=1:4
subplot(4,1,i)
plot(t,measure(:,i),'b-','LineWidth',1.25)
hold on
plot(tmodel(:,i),model(:,i),'r--','LineWidth',1.25)
title(['Gauge ' gauge(i,:)])
ylabel('eta (m)')
grid
axis(ax);
if i == 4
xlabel('x (m)')
end
end
