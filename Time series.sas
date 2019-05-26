DATA timeseries;
INFILE "/home/albam10240/timeseries2.csv" DELIMITER=',' FIRSTOBS=2;
INPUT spr econ vol kospi frgn br prem ff;
RUN;

data new;
infile '/home/albam10240/new.txt'; 
input cds;
time=_n_;
run;

data series;
merge timeseries new;
run;

data series;
set series;
drop br prem;
run;

proc print data=series;
run;

DATA trans;
SET timeseries;
time = _N_;
trspr = 1 / SQRT(3.5 - spr);
trskospi = LOG(kospi);
trsff=log(ff);
IF time = 45 THEN pt1=1; ELSE pt1=0; /* 사용 */
IF time < 45 THEN st1=0; ELSE st1=1; /* 사용 */
RUN;

PROC ARIMA DATA=trans;
IDENTIFY VAR=econ(1); /* X1 - 경기동행지수 */
ESTIMATE p=(3) q=(1 2)(12);
IDENTIFY VAR=trskospi(1); /* X2 - 코스피지수 */
ESTIMATE p=(1 2) q=(1 2)(3) NOCONSTANT MAXITER=500;
IDENTIFY VAR=trspr(1) CROSSCORR=(trskospi(1) econ(1)) nlag=24;
estimate p=(1 3 5) q=(3 6)(12) input=((2 3 4)/(1 2)econ) maxiter=3000;
forecast lead=4 out=fore_econ;
RUN;

data fore_econ;
merge fore_econ trans;
forecast=3.5-1/(forecast)**2; u95=3.5-1/(u95)**2; l95=3.5-1/(l95)**2;
run;

data fore_econ;
set fore_econ;
if time<48 then delete;
run;

%fore_p(fore_econ, 2009, 1, 1, spr);

data new;
infile '/home/albam10240/new.txt'; 
input cds;
time=_n_;
run;


data trans;
merge trans new;
run;

%origin_p(trans, 2005, 1, 1, trsff);
%origin_p(trans, 2005, 1, 1, cds);
%origin_p(trans, 2005, 1, 1, vol);
%origin_p(trans, 2005, 1, 1, spr);

proc arima data=trans;
identify var=trsff(1) nlag=48;
estimate p=(2 3 10) q=(1 2 3);
run;

proc arima data=trans;
identify var=cds(1);
estimate p=(6 7) q=(4 6 7) noconstant;
run;

proc arima data=trans;
identify var=vol(1);
estimate p=1 q=1 noconstant;
run;

/* 4-24/ 2 : ff */
/* 6-14/ 2 : cds*/

/* econ : 2~3   kospi : 0~4 */

/* economic model */
proc arima data=trans;
IDENTIFY VAR=econ(1); /* X1 - 경기동행지수 */
ESTIMATE p=(3) q=(1 2)(12);
IDENTIFY VAR=trskospi(1); /* X2 - 코스피지수 */
ESTIMATE p=(1 2) q=(1 2)(3) NOCONSTANT MAXITER=500;
identify var=spr(1) crosscorr=(econ(1) trskospi(1)) nlag=48;
estimate p=(1 3 4) q=(24)(6) input=(2$(1)econ (2 4)/(2)trskospi) noconstant;
forecast lead=4 out=econ_m;
run;

data econ_m;
set econ_m;
time=_n_;
if time<37 then delete;
run;

%fore_p(econ_m, 2005, 1, 1, spr);


/* non-economic model */
proc arima data=trans;
identify var=trsff(1) nlag=48;
estimate p=(2 3 10) q=(1 2 3);
identify var=cds(1);
estimate p=(6 7) q=(4 6 7) noconstant;
identify var=vol(1);
estimate p=1 q=1 noconstant;
identify var=spr(1) crosscorr=(trsff(1) cds(1) vol(1)) nlag=48;
estimate p=(3 15) q=(1 2)
input=(4$(2 5 7 12 13 18 19)/(1 2)trsff
6$(3 7)/(2)cds) maxit=300 noconstant;
forecast lead=4 out=non_m;
run;

%fore_p(non_m, 2005, 1, 1, spr);




/* 4-24/ 2 : ff */
/* 6-14/ 2 : cds*/
/* econ : 2~3   kospi : 0~4 */

/* full model */
proc arima data=trans;
IDENTIFY VAR=econ(1); /* X1 - 경기동행지수 */
ESTIMATE p=(3) q=(1 2)(12);
IDENTIFY VAR=trskospi(1); /* X2 - 코스피지수 */
ESTIMATE p=(1 2) q=(1 2)(3) NOCONSTANT MAXITER=500;
identify var=trsff(1) nlag=48;
estimate p=(2 3 10) q=(1 2 3);
identify var=cds(1);
estimate p=(6 7) q=(4 6 7) noconstant;
identify var=spr(1) crosscorr=(econ(1) trskospi(1) trsff(1) cds(1)) nlag=48;
estimate p=(4 6) q=(1 2)
input=(4$(3 5 11 12 13 21)/(1 2)trsff
6$(3 7)/(2)cds 2$econ (4)/(2)trskospi) maxit=500 noconstant;
forecast lead=4 out=full;
run;

%fore_p(full, 2005, 1, 1, spr);

/* Pre whitening & crosscorrelation */

proc arima data=trans;
identify var=trsff(1) nlag=48;
estimate p=(2 3 10) q=(1 2 3);
identify var=cds(1) nlag=48;
estimate p=(6 7) q=(4 6 7) noconstant;
identify var=vol(1) nlag=48;
estimate p=1 q=1 noconstant;
IDENTIFY VAR=econ(1) nlag=48; /* X1 - 경기동행지수 */
ESTIMATE p=(3) q=(1 2)(12);
IDENTIFY VAR=trskospi(1) nlag=48; /* X2 - 코스피지수 */
ESTIMATE p=(1 2) q=(1 2)(3) NOCONSTANT MAXITER=500;
identify var=spr(1) crosscorr=(trskospi(1) econ(1));
identify var=spr(1) crosscorr=(cds(1) vol(1));
identify var=spr(1) crosscorr=(trsff(1));
run;

proc arima data=trans;
identify var=trsff(1) nlag=48;
estimate p=(2 3 10) q=(1 2 3);
forecast lead=4 out=ff;
run;

proc arima data=trans;
identify var=cds(1) nlag=48;
estimate p=(6 7) q=(4 6 7) noconstant;
forecast lead =4 out=cds;
run;

proc arima data=trans;
identify var=vol(1) nlag=48;
estimate p=1 q=1 noconstant;
forecast lead=4 out=vol;
run;

proc arima data=trans;
IDENTIFY VAR=econ(1) nlag=48; /* X1 - 경기동행지수 */
ESTIMATE p=(3) q=(1 2)(12);
forecast lead=4 out=econ;
run;

proc arima data=trans;
IDENTIFY VAR=trskospi(1) nlag=48; /* X2 - 코스피지수 */
ESTIMATE p=(1 2) q=(1 2)(3) NOCONSTANT MAXITER=500;
forecast lead=4 out=kos;
run;

data ff;
set ff;
forecast=exp(forecast); u95=exp(u95); l95=exp(l95);
run;

data kos;
set kos;
forecast=exp(forecast); u95=exp(u95); l95=exp(l95);
run;

data time1;
merge trans ff; run;

data time2;
merge trans kos; run;


%fore_p(time2, 2005, 1, 1, kospi);
%fore_p(time1, 2005, 1, 1, ff);
%fore_p(vol, 2005, 1, 1, vol);
%fore_p(econ, 2005, 1, 1, econ);
%fore_p(cds, 2005, 1, 1, cds);


%origin_p(trans, 2005, 1, 1, spr);
%origin_p(trans, 2005, 1, 1, cds);
proc arima data=trans;
identify var=trspr crosscorr=(st1);
estimate input=(4$/(1)st1);
forecast out=spr;
run;

%origin_p(trans, 2005, 1, 1, spr);

