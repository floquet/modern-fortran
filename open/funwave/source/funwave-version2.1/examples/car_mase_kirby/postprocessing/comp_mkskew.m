% Program mkskew.m. Make-Kirby (1992) experiment.
% To compare the computed skewness and asymmetry of 
% surface elevation with the standard result of 
% Example 4.1.

clear all
%'loading standard data'

mkskew=load('mkskew_man.out');

ske=mkskew(1,:);  %skewness
ae=mkskew(2,:);   %asymmetry

'loading numerical results'
%nnlth = 35840;
%fid = fopen('etats.out');
%en  = fread(fid,[nnlth,13],'float');  en=100*en;

%fdir='/Users/fengyanshi/tmp4/';
fdir='';

for k=1:12
num=sprintf('%.2d',k);
gage=load([fdir 'sta_00' num]);
en(:,k)=gage(:,2)*100;
end


'Computing'

% compute statistics  
% remove the first 20 second data; 

en = en(400:1:length(en),:);  
%en = en(400:1:7200,:); 
nn = length(en);  

h=[47 35 30 25 20 17.5 15 12.5 10 7.5 5 2.5]; nh = 12; nhn = 11;

% remove mean
enn = en - ones(nn,1)*mean(en);

%standard deviation, skewness
stn = std(enn);
skn = sum(enn.^3)/nn./stn.^3;

% Hilbert transform
hn  = imag(hilbert(enn));
hnn = hn-ones(nn,1)*mean(hn);  
 
%asymmetry
an  = -sum(hnn.^3)/nn./std(hn).^3;

save skew_asy.mat h ske skn ae an

% manual results
manu=load('mkskew_kirby.out');
mskew=manu(1,:);
mskew=mskew;
masy=manu(2,:);
masy=masy;

n=1:12;
nn=2:12;
plot(h(n),ske(n),'k-o',h(nn),skn(nn),'k--o', ...
h(nn),mskew(nn),'k-.o', ...
     h(n),ae(n),'k-x',...
h(nn),an(nn),'k--x',...
h(nn),masy(nn),'k-.x')
axis([0 50 -0.25 1.2]); grid
xlabel('Water Depth (cm)')
ylabel('Skewness (o) and Asymmetry (x)')
%title('Comparison of skewness (o) and asymmetry (*)  data (-)  model (--)')
legend('skewness (data)','skewness (present model)','skewness (Kirby et al.)', 'asymmetry (data)','asymmetry (present model)','asymmetry (Kirby et al.)')

% rmse
sk_rmse_tvd=rmse(skn(nn),ske(nn))/max(ske)*100
sk_rmse_kirby=rmse(mskew(nn),ske(nn))/max(ske)*100

as_rmse_tvd=rmse(an(nn),ae(nn))/max(ae)*100
as_rmse_kirby=rmse(masy(nn),ae(nn))/max(ae)*100


%print skmk.ps
