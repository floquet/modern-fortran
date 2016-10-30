clf
clear all
sk=2;
fact=1;

set(gcf,'units','inches','paperunits','inches','papersize', [8 3.5],'position',[1 1 9 4.5],'paperposition',[0 0 8 3.5]);

datae=load('FSE_BM2_shi_tvd.txt');
% shift time
    [m,n]=size(datae);
    t=datae(:,1);
    eta=datae(:,2:n);

        WGdata=load('WG1.txt');

            for k=1:length(eta(:,1))
                if eta(k,1)==max(eta(:,1))
                    num_shift_ind=k;
                    num_t_shift=t(k);
                    break
                end
            end
            
            for k=1:length(WGdata(:,2))
                if WGdata(k,2)==max(WGdata(:,2))
                    exp_shift_ind=k;
                    exp_t_shift=WGdata(k,1);
                    break
                end
            end   
        
            t=t-num_t_shift+exp_t_shift; % some difference between eta and u

% read
 data=load('VT_C_BM2_shi_tvd.txt');       
    [m,nn]=size(data);
    n=6;
    vel=zeros(m,6);
    vel(:,1:nn-1)=data(:,2:nn);
    veldata=load('U_Velocity_AverageC.txt');

subplot(211)
plot(veldata(1:sk:length(veldata),1),veldata(1:sk:length(veldata),2)-mean(veldata(1:25,2)),'k-')
       
hold on
plot(t,vel(:,1)/fact,'k--')
    ylabel('U (m/s)')
grid
axis([0 47 -2.0 3.5])
% V
 data=load('VT_C_BM2_shi_tvd.txt');       
    [m,nn]=size(data);
    n=6;
    vel=zeros(m,6);
    vel(:,1:nn-1)=data(:,2:nn);
    veldata=load('V_Velocity_AverageC.txt');
text(24,1.5,'ADV C')

legend('data','model')

subplot(212)

plot(veldata(1:sk:length(veldata),1),veldata(1:sk:length(veldata),2)-mean(veldata(1:25,2)),'k-')
       
hold on
% here measurement has 0.25 lag
plot(t,vel(:,2)/fact,'k--')
grid
    ylabel('V (m/s)')
text(24,1.5,'ADV C')
axis([0 47 -2. 3.5])
xlabel('Time (s)')

