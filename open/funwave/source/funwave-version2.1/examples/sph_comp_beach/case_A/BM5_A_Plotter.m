clear all
load('A.mat');

n4=load('sta_0001');
n5=load('sta_0002');
n6=load('sta_0003');
n7=load('sta_0004');
n8=load('sta_0005');
n9=load('sta_0006');
n10=load('sta_0007');

shift=265.82;
%t=t-263.82;
for i=1:length(n4)
    n4(i,1)=n4(i,1)/10.+shift;
    n5(i,1)=n5(i,1)/10.+shift;
    n6(i,1)=n6(i,1)/10.+shift;
    n7(i,1)=n7(i,1)/10.+shift;
    n8(i,1)=n8(i,1)/10.+shift;
    n9(i,1)=n9(i,1)/10.+shift;
    n10(i,1)=n10(i,1)/10.+shift;

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

skp=2;

n4(:,2)=n4(:,2)+6*skp;
g4=g4+6*skp;

n5(:,2)=n5(:,2)+5*skp;
g5=g5+5*skp;

n6(:,2)=n6(:,2)+4*skp;
g6=g6+4*skp;

n7(:,2)=n7(:,2)+3*skp;
g7=g7+3*skp;

n8(:,2)=n8(:,2)+2*skp;
g8=g8+2*skp;

n9(:,2)=n9(:,2)+1*skp;
g9=g9+1*skp;

%n10(:,2)=n10(:,2)+20;
%g10=g10+2;

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
title('Case B (H/d=0.0.0378)')
axis([268 295 -1 14])
xlabel('sec')
ylabel('cm')
grid on
%set(gca,'XTick',270:2:286)
%set(gca,'YTick',0:0.6:7.8)

%hold on
%plot(tw,wall)
%hold on
%plot(w(:,1),w(:,2),'r')
