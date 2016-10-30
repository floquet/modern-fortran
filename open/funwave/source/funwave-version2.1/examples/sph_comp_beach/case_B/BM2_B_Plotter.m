clear all
load('B.mat');

n4=load('sta_0001');
n5=load('sta_0002');
n6=load('sta_0003');
n7=load('sta_0004');
n8=load('sta_0005');
n9=load('sta_0006');
n10=load('sta_0007');


%t=t-263.82;
for i=1:length(n4)
    n4(i,1)=n4(i,1)/10.+263.82;
    n5(i,1)=n5(i,1)/10.+263.82;
    n6(i,1)=n6(i,1)/10.+263.82;
    n7(i,1)=n7(i,1)/10.+263.82;
    n8(i,1)=n8(i,1)/10.+263.82;
    n9(i,1)=n9(i,1)/10.+263.82;
    n10(i,1)=n10(i,1)/10.+263.82;

end

for i=1:length(n4)
    n4(i,2)=n4(i,2);
    n5(i,2)=n5(i,2);
    n6(i,2)=n6(i,2);
    n7(i,2)=n7(i,2);
    n8(i,2)=n8(i,2);
    n9(i,2)=n9(i,2);
    n10(i,2)=n10(i,2);
end

for i=1:600
    g4(i)=g4(i)*100;
    g5(i)=g5(i)*100;
    g6(i)=g6(i)*100;
    g7(i)=g7(i)*100;
    g8(i)=g8(i)*100;
    g9(i)=g9(i)*100;
    g10(i)=g10(i)*100;
end

n4(:,2)=n4(:,2)+60;
g4=g4+60;

n5(:,2)=n5(:,2)+50;
g5=g5+50;

n6(:,2)=n6(:,2)+40;
g6=g6+40;

n7(:,2)=n7(:,2)+30;
g7=g7+30;

n8(:,2)=n8(:,2)+20;
g8=g8+20;

n9(:,2)=n9(:,2)+10;
g9=g9+10;

%n10(:,2)=n10(:,2)+20;
%g10=g10+20;

%subplot(7,1,1);
plot(t,g4)
hold on
plot(n4(:,1),n4(:,2),'r')
hold on
%subplot(7,1,2);
plot(t,g5)
hold on
plot(n5(:,1),n5(:,2),'r')
hold on
%subplot(7,1,3);
plot(t,g6)
hold on
plot(n6(:,1),n6(:,2),'r')
hold on
%subplot(7,1,4);
plot(t,g7)
hold on
plot(n7(:,1),n7(:,2),'r')
hold on
%subplot(7,1,5);
plot(t,g8)
hold on
plot(n8(:,1),n8(:,2),'r')
hold on
%subplot(7,1,6);
plot(t,g9)
hold on
plot(n9(:,1),n9(:,2),'r')
hold on
%subplot(7,1,7);
plot(t,g10)
hold on
plot(n10(:,1),n10(:,2),'r')
title('Case B (H/d=0.2578)')
axis([268 287 -2 70])
xlabel('sec')
ylabel('cm')
grid on
set(gca,'XTick',270:2:286)
%set(gca,'YTick',0:0.6:7.8)

%hold on
%plot(tw,wall)
%hold on
%plot(w(:,1),w(:,2),'r')
