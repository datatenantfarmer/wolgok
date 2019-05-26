data dw;
infile '/home/albam10240/DWKOSPI.csv' firstobs=2 delimiter=',';
input kospi dw;
run;

data dw2;
set dw;
ret_ko=dif(log(kospi));
ret_dw=dif(log(dw));
time=_n_;
run;

proc sgplot data=dw2;
 series x=time y=dw;
run;

proc sgplot data=dw2;
 series x=time y=kospi;
 run;
 
proc sgplot data=dw2;
series x=time y=ret_ko;
series x=time y=ret_dw;
run;

/* recomend p : n**1/3 */
/* Autoregressive model of error terms */
proc autoreg data=dw2;
model ret_dw = ret_ko / nlag=(1 2 3 4 5 6 7) backstep;
model ret_dw = ret_ko / nlag=(1);
output out=resid_out r=resid p=pred lcl=l95 ucl=u95;
run; 

proc arima data=resid_out;
identify var=resid nlag=48;
run;

proc autoreg data=dw2;
model ret_dw = ret_ko / nlag = 36 backstep;
model ret_dw = ret_ko / nlag= (1 27 30 32);
output out=resid_out r=resid p=pred lcl=l95 ucl=u95;
run;

proc arima data=resid_out;
identify var=resid nlag=48;
run;

proc sgplot data=resid_out;
series x=time y=ret_dw;
series x=time y=pred;
series x=time y=l95;
series x=time y=u95;
run;

/* prediction is poor */

/* arch/ garch model */

data resid_out;
set resid_out;
res2=resid**2;
run;

proc arima data=resid_out;
identify var=res2 nlag=60;
run;

/* normality test is rejected */
proc autoreg data=dw2;
model ret_dw=ret_ko/ nlag=(1 27 30 32) garch=(p=1, q=1) dist=t;
output out=garch_out r=resid p=pred lcl=l95 ucl=u95 cev=ht1 cpev=ht2;
run;

/* test autocorrelation of at and squared at */
data garch_out;
set garch_out;
at=resid/sqrt(ht1); 
at2=at**2;
run;

proc arima data=garch_out;
identify var=at nlag=60;
identify var=at2 nlag=60;
run;

/* arch1 + garch1 ~ 1 -> IGARCH */
proc autoreg data=dw2;
model ret_dw = ret_ko/nlag=(1 30 32) garch=(p=2, q=1, type=integ, noint) dist=t;
output out=garch_out r=resid p=pred lcl=l95 ucl=u95 cev=ht1 cpev=ht2;
run;

data garch_out;
set garch_out;
at=resid/sqrt(ht1);
at2=at**2;
run;

proc arima data=garch_out;
identify var=at nlag=60;
identify var=at2 nlag=60;
run;

/* IGARCH(1,2) is adequate */

/* Is there premium? Null : no premium*/
proc autoreg data=dw2;
model ret_dw=ret_ko/ nlag=(1 30 32) garch=(p=2, q=1, type=integ, mean=linear) dist=t;
model ret_dw=ret_ko/ nlag=(1 30 32) garch=(p=2, q=1, type=integ, mean=log) dist=t;
model ret_dw=ret_ko/ nlag=(1 30 32) garch=(p=2, q=1, type=integ, mean=sqrt) dist=t;
run;

/* there is no premium */

/* there is leverage effect? */
%macro leverage;;
data leverage; set garch_out(keep=at2 at rename=(at2=at_2));
%do i=1%to 36; at&i=lag&i(AT); %end; run;
%mend leverage;
%leverage;

proc reg data=leverage; model at_2=at1-at36; run;

/* there is leverage effect */

/* EGARCH : Unsymmetric model : Ignore normality test*/
proc autoreg data=dw2;
model ret_dw = ret_ko / nlag=(27 30 32) garch=(p=1, q=1, type=exp) maxiter=100;
output out=garch_out r=resid p=pred lcl=l95 ucl=u95 cev=ht1 cpev=ht2;
run;

data garch_out;
set garch_out;
at=resid/sqrt(ht1);
at2=at**2;
run;

proc arima data=garch_out;
identify var=at nlag=60;
identify var=at2 nlag=60;
run;

proc sgplot data=resid_out;
series x=time y=ret_dw;
series x=time y=pred;
series x=time y=l95;
series x=time y=u95;
run;
