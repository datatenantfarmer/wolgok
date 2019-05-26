data spr;
infile '/home/albam10240/econ.txt' expandtabs;
input spr call exinfl build fore bok exch fis bokfrac;
spr = 100*spr;
ty=1/sqrt(100+1-spr);
tb=log(build);
tbok=sqrt(bok);
tex=sqrt(exch);
tf=sqrt(fis);
run;

proc arima data=spr;
identify var=spr(1 1);
run;

proc arima data=spr;
identify var=ty;
run;

proc arima data=spr;
identify var=call(1 1);
estimate p=(1 2) q=(1);
estimate p=(1 2)(18) q=(1)(18);
estimate p=(1 2)(18) q=(1)(18) noconstant;
estimate p=(2)(18) q=(1)(18) noconstant;
estimate p=(2)(18) q=(1) noconstant;
estimate p=(18) q=(1) noconstant;
run;

proc arima data=spr;
identify var=exinfl(1 12);
estimate p=(1 2)(12) q=(1 2)(12);
estimate p=(1 2) q=(1)(12) noconstant;
estimate p=(1 2)(5) q=(1)(12) noconstant;
estimate p=(2)(5) q=(12) noconstant;
run;

proc arima data=spr;
identify var=tb(1);
estimate p=(1 2 3) q=(1 2 3);
estimate p=(1 2 3) q=(1 2 3) noconstant;
estimate p=(1 2 3) q=(1 2) noconstant ml;
run;

proc arima data=spr;
identify var=fore(1 1);
estimate p=(1 2 3 4 5 6 7 8 9) q=(1);
estimate p=(1 2 3 4 5 6 7 8 9)(12) q=(1);
estimate p=(1 2 3 4 5 8)(12) q=(1) noconstant;
estimate p=(4 8)(12) q=(1) noconstant;
estimate p=(4 8) q=(1)(18) noconstant;
run;

proc arima data=spr;
identify var=tbok(1 6);
estimate p=(1 2)(6) q=(1 2)(6);
estimate p=(1 2)(6) q=(1 2)(6) noconstant;
run;

proc arima data=spr;
identify var=tex(1);
estimate p=(1 2 3 4) q=(1);
estimate p=(1 2 3 4) noconstant;
estimate p=(1 2 4) noconstant;
run;

/*
proc arima data=spr;
identify var=fis(12 1) nlag=48;
estimate p=(1 2)(12) q=(1 2 3 4 5)(12);
estimate p=(1 2)(12) q=(1 2 4 5)(12);
estimate p=(1 2) q=(2 5)(12);
estimate p=(1) q=(2 5)(12);
estimate q=(2 5)(12);
estimate q=(2)(12);
estimate q=(2)(12) noconstant;
estimate q=(2)(12) noconstant;
run;
*/

proc arima data=spr;
identify var=tf(12 1) nlag=48;
estimate p=(5)(12) q=(5)(12);
estimate p=(12) q=(5)(12);
estimate q=(5)(12);
estimate q=(5)(12) noconstant;
run;

proc arima data=spr;
identify var=bokfrac(1);
estimate p=(1 2 3) q=(1 2 3);
estimate p=(1 2 3) q=(3);
estimate p=(2 3);
estimate p=(2 3) noconstant;
run;


proc arima data=spr;
identify var=call(1 1);
estimate p=(18) q=(1) noconstant;
identify var=exinfl(1 12);
estimate p=(2)(5) q=(12) noconstant;
identify var=tb(1);
estimate p=(1 2 3) q=(1 2) noconstant ml;
identify var=fore(1 1);
estimate p=(4 8) q=(1)(18) noconstant;
identify var=tbok(1 6);
estimate p=(1 2)(6) q=(1 2)(6) noconstant;
identify var=tex(1);
estimate p=(1 2 4) noconstant;
identify var=tf(12 1) nlag=48;
estimate q=(5)(12) noconstant;
identify var=bokfrac(1);
estimate p=(2 3) noconstant;
identify var=spr(1 1) 
	crosscorr=(call(1 1) exinfl(1 12) tb(1) fore(1 1) tbok(1 6) tex(1) tf(12 1) bokfrac(1));
run;