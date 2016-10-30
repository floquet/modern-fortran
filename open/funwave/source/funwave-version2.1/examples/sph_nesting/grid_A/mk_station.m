west=zeros(42,2);
west(1:21,1)=80;
west(1:21,2)=[81:1:101];
west(22:42,1)=[81:1:101];
west(22:42,2)=80;
fid = fopen('station.txt', 'wt');
  fprintf(fid, ['%5d','%5d', '\n'], west');
  fclose(fid);

%save -ASCII station.txt west