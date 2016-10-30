clf
clear all

fact=1;               
sk=8; 

set(gcf,'units','inches','paperunits','inches','papersize', [6 7],'position',[1 1 7 8],'paperposition',[0 0 6 7]);

data=load('FSE_BM2_shi_tvd.txt');

% shift time
    [m,n]=size(data);
    t=data(:,1);
    eta=data(:,2:n);

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
            
            t=t-num_t_shift+exp_t_shift;


WG(:,1)=WGdata(:,2);
WGdata=load('WG2.txt');
WG(:,2)=WGdata(:,2);
WGdata=load('WG3.txt');
WG(:,3)=WGdata(:,2);
WGdata=load('WG4.txt');
WG(:,4)=WGdata(:,2);
WGdata=load('WG5.txt');
WG(:,5)=WGdata(:,2);
WGdata=load('WG6.txt');
WG(:,6)=WGdata(:,2);
WGdata=load('WG7.txt');
WG(:,7)=WGdata(:,2);
WGdata=load('WG8.txt');
WG(:,8)=WGdata(:,2);
WGdata=load('WG9.txt');
WG(:,9)=WGdata(:,2);

xwidth  = 0.85;

yheight = 0.08;
ymargin = 0.0225;
x1=0.1;
obs_line_width = 2;
obs_color= 'k';
BO_style = '--';
BO_line_width = 1.0;


for i = 1:9

subplot('Position',[ x1   0.885-(i-1)*yheight-(i-1)*ymargin   xwidth,  yheight])

    h1 = plot(WGdata(1:sk:length(WGdata),1),WG(1:sk:length(WGdata),i)/100.,'-');
%set(h1,'Color',obs_color)
    set(h1,'Color',obs_color,'MarkerSize',obs_line_width)
    hold on
    h2 = plot(t,eta(:,i),'-');
set(h2,'Color',obs_color,'LineStyle',BO_style)
%   set(h2,'Color',obs_color,'LineStyle',BO_style,'LineWidth',BO_line_width)
ylabel('\eta (m)')
axis([0 47 -0.2 0.5])

if i==1 
legend('data','model')
end

text(24,0.3,['Gauge ' num2str(i)])

    if i==9

    else
 set(gca,'XTickLabel',''); ;
    end

grid
end
 
xlabel('Time (s)')

break





%print -depsc2 WG1_comp.eps
