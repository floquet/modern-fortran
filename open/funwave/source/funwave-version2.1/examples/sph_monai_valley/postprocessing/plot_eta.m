
clear all

fdir='/Users/fengyanshi/tmp3/';
nstart=input('nstart=');
nend=input('nend=');

%set(gcf,'units','inches','paperunits','inches','papersize', [5 9],'position',[1 1 6 10],'paperposition',[0 0 5 9]);

icount=0;
for num=nstart:1:nend
icount=icount+1;

fname=sprintf('%.4d',num);
ele=load([fdir 'eta_' fname]);
mask=load([fdir 'mask_' fname]);
ele(mask<1)=NaN;
clf

pcolor(ele),shading interp
colorbar

M(:,icount)=getframe(gcf);
pause(0.1)
end
%movie2avi(M,'tmp.avi','fps',10)
