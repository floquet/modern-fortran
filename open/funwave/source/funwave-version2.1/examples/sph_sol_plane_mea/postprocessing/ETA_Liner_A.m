clear all
fdir='';
%--------------------load files--------------
dep=100;

d=load([fdir 'eta_0095']);
n=length(d);
d1=d(2,:);
d=load([fdir 'eta_0126']);
d2=d(2,:);
d=load([fdir 'eta_0157']);
d3=d(2,:);
d=load([fdir 'eta_0189']);
d4=d(2,:);
d=load([fdir 'eta_0219']);
d5=d(2,:);


%--------------------------------------------

dx=5;
for i=1:n
    x(i)=(6000.0-(i-1)*dx)/dep;
end

ii=0;
for i=n:-1:1
    if x(i)>=-2.0
        if x(i)<=20.0
            ii=ii+1;
            kk=i;
            data(ii,2)=d1(i)/dep;
            data(ii,3)=d2(i)/dep;
            data(ii,4)=d3(i)/dep;
            data(ii,5)=d4(i)/dep;
            data(ii,6)=d5(i)/dep;
        end
    end
end
ll=0;
for i=ii:-1:1
    ll=ll+1;
    data(ll,1)=x(i+kk-1);
end

dlmwrite('FUNWAVE_BP1_SHAPE.txt', data,'delimiter', '\t','precision', 6);


