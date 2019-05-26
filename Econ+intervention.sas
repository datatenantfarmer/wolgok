/*** 데이터 불러오기 ***/

DATA timeseries;
INFILE "/home/albam10240/timeseries2.csv" DELIMITER=',' FIRSTOBS=2;
INPUT spr econ vol kospi frgn br prem ff;
RUN;

/*** 변수변환 및 개입변수 설정 ***/

DATA trans;
SET timeseries;
time = _N_;
trspr = 1 / SQRT(3.5 - spr);
trskospi = LOG(kospi);
if time<31 then sst2=0; else sst2=1;
IF time = 44 THEN pt1=1; ELSE pt1=0; /* 사용 */
IF time < 45 THEN st1=0; ELSE st1=1; /* 사용 */
IF time < 52 THEN st2=0; ELSE st2=1;
if time=29 then ppt=1; else ppt=0;
if time<29 then sst=0; else sst=1;
RUN;

proc arima data=trans out=ff;
identify var=ff crosscorr=(st2 sst2);
estimate input=(/(1)st2 /(1)sst2) maxiter=2000;
forecast;
run;

DATA ff;
SET ff;
RENAME residual=ffr;
DROP ff forecast std l95 u95;
RUN;

PROC ARIMA DATA=trans out=vol;
IDENTIFY VAR=vol CROSSCORR=(pt1) NOPRINT; /* X4 - 국고채변동성 */
ESTIMATE INPUT=(1$/(1)pt1);
FORECAST;
RUN;

DATA vol;
SET vol;
RENAME residual=volr;
DROP vol forecast std l95 u95;
RUN;

DATA trans;
MERGE trans ff vol;
RUN;

PROC ARIMA DATA=trans;
IDENTIFY VAR=volr;
ESTIMATE P=1 q=1;
RUN;

PROC ARIMA DATA=trans;
IDENTIFY VAR=volr;
ESTIMATE p=1 q=1;
identify var=ffr(1);
estimate p=(3) q=(3 10) maxiter=2000  noconstant;
IDENTIFY VAR=trspr(1) CROSSCORR=(st1(1) volr ffr(1)) NLAG=13;
ESTIMATE INPUT=(4$/(1)st1 (1 2 3 4 5 6 7 8)/(1 2)ffr);
ESTIMATE INPUT=(4$/(1)st1 (1 3 4 5 6 8)/(2)ffr);
ESTIMATE INPUT=(4$/(1)st1 (1 3 5 6 8)/(2)ffr) NOCONSTANT;
ESTIMATE INPUT=(4$/(1)st1 (1 5 6 8)/(2)ffr) NOCONSTANT;
ESTIMATE INPUT=(4$/(1)st1 (5 6 8)/(2)ffr) NOCONSTANT;
ESTIMATE p=(1 2 3 4 5) q=(2) INPUT=(4$/(1)st1 (5 6 8)/(2)ffr) NOCONSTANT;
ESTIMATE p=(1 3 4 5) q=(2) INPUT=(4$/(1)st1 (5 6 8)/(2)ffr) NOCONSTANT;
forecast lead=4 out=shit;
RUN;

proc arima data=ff;
identify var=ffr(1);
estimate p=(3) q=(3 10) maxiter=2000  noconstant;
forecast lead=4 out=l;
run;

%fore_p(l, 2005, 1, 1, ffr);


data shit;
merge shit trans;
forecast=3.5-1/(forecast)**2; u95=3.5-1/(u95)**2; l95=3.5-1/(l95)**2;
run;

%fore_p(shit, 2005, 1, 1, spr);
