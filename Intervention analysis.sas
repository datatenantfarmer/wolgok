data unemp;
infile '/home/albam10240/표10.2-실업률변화추이자료.txt';
input unemp;
time=_n_;
t_unemp=sqrt(unemp);
run;

%origin_p(unemp, 1991, 1, 1, t_unemp);

data int;
set unemp;
if time=84 then pt=1; else pt=0;
if time<84 then st=0; else st=1;
run;

proc arima data=int;
identify var=t_unemp crosscorr=(pt) noprint;
estimate input=(1$/(1)pt);
forecast out=resi;
run;

%origin_p(resi, 1991, 1,1, residual);

data resi;
set resi;
resi12=dif12(residual);
resi121=dif(resi12);
run;

%origin_p(resi, 1991, 1, 1, resi121);

proc arima data=int;
identify var=t_unemp(1 12) crosscorr=(pt(1 12)) noprint;
estimate p=(1 2)(12 24) input=(1$/(1)pt) noconstant maxit=100;
run;

data gas;
infile '/home/albam10240/표2.7-전라북도지역도시가스판매량자료.txt';
input time gas;
run;

data gas;
set gas;
month=mod(time, 12);
run;

%origin_p(gas, 2005, 1,1, gas);

data gas;
set gas;
if month=1 then jan=1; else=0;
if month=2 then fab=1; else=0;
if month=3 then mar=1; else=0;
if month=4 then apr=1; else=0;
if month=5 then may=1; else=0;
if month=6 then jun=1; else=0;
if month=7 then jul=1; else=0;
if month=8 then aug=1; else=0;
if month=9 then sep=1; else=0;
if month=10 then oct=1; else=0;
if month=11 then nov=1; else=0;
if month=0 then dec=1; esle=0;
run;

proc arima data=gas;
identify var=gas crosscorr=(time jan fab mar apr may jun jul aug sep oct nov dec) noprint;
estimate input=(time jan fab mar apr may jun jul aug sep oct nov dec);
run;

data bul;
infile '/home/albam10240/표7.4-우리나라연탄생산량자료.txt';
input year quar bul;
time=_n_;
run;

%origin_p(bul, 2005, 1, 4, bul);

data bul;
set bul;
t=time;
t2=t**2;
p1=0; p2=0; p3=0;
if quar=1 then p1=1; else if quar=2 then p2=1; else if quar=3 then p3=1;
run;

proc arima data=bul;
identify var=bul crosscorr=(t t2 p1 p2 p3) noprint;
estimate input=(t t2 p1 p2 p3);
forecast lead=3 out=fore_bul;
run;

%fore_p(fore_bul, 2005, 1, 4, bul);