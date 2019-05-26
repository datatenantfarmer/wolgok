/* Ebay stock return */
data ebay;
infile '/home/albam10240/ebay.txt';
input return;
time=_n_;
run;

proc arima data=ebay;
identify var=return;
run;

proc autoreg data=ebay;
model return=/nlag=(8 29 39 47) garch=(p=1, q=1) dist=t maxiter=500;
output out=resid r=resid p=pred ucl=u95 lcl=l95 cev=ht1;
run;

data resid;
set resid;
at=resid/sqrt(ht1);
at2=at**2;
run;

proc arima data=resid;
identify var=at nlag=60;
identify var=at2 nlag=60;
run;

proc sgplot data=resid;
series x=time y=return;
series x=time y=pred;
series x=time y=u95;
series x=time y=l95;
run;

/* IGARCH */
proc autoreg data=ebay;
model return=/nlag=(8 29 39 47) garch=(p=1, q=1, type=integ) dist=t maxiter=500;
output out=resid r=resid p=pred ucl=u95 lcl=l95 cev=ht1;
run;

data resid;
set resid;
at=resid/sqrt(ht1);
at2=at**2;
run;

proc arima data=resid;
identify var=at nlag=60;
identify var=at2 nlag=60;
run;

proc sgplot data=resid;
series x=time y=return;
series x=time y=pred;
series x=time y=u95;
series x=time y=l95;
run;

proc autoreg data=ebay;
model return=/nlag=(8 29 39 47) garch=(p=1, q=1, type=integ, mean=linear) dist=t maxiter=500;
model return=/nlag=(8 29 39 47) garch=(p=1, q=1, type=integ, mean=log) dist=t maxiter=500;
model return=/nlag=(8 29 39 47) garch=(p=1, q=1, type=integ, mean=sqrt) dist=t maxiter=500;
output out=resid r=resid p=pred ucl=u95 lcl=l95 cev=ht1;
run;

/* there is no premium */
proc autoreg data=ebay;
model return=/nlag=(8 29 39 47) garch=(p=1, q=1, type=integ) dist=t maxiter=500;
output out=resid r=resid p=pred ucl=u95 lcl=l95 cev=ht1;
run;

data resid;
set resid;
at=resid/sqrt(ht1);
at2=at**2;
run;

/* leverage effect? */

%macro leverage;;
data leverage; set resid(keep=at2 at rename=(at2=at_2));
%do i=1%to 36; at&i=lag&i(AT); %end; run;
%mend leverage;
%leverage;

proc reg data=leverage; model at_2=at1-at36; run;

proc autoreg data=ebay;
model return=/nlag=(8 29 39 47) garch=(p=1, q=1, type=exp) maxiter=500;
output out=resid r=resid p=pred ucl=u95 lcl=l95 cev=ht1;
run;

/* there is no leverage effect */

/* final model */
proc autoreg data=ebay;
model return=/nlag=(8 29 39 47) garch=(p=1, q=1, type=integ) dist=t maxiter=500;
output out=resid r=resid p=pred ucl=u95 lcl=l95 cev=ht1;
run;

data resid;
set resid;
at=resid/sqrt(ht1);
at2=at**2;
run;

proc arima data=resid;
identify var=at nlag=60;
identify var=at2 nlag=60;
run;

proc sgplot data=resid;
series x=time y=return;
series x=time y=pred;
series x=time y=u95;
series x=time y=l95;
run;


/* spread : Absence of arch effect*/
data spr5;
infile '/home/albam10240/spr5.txt';
input return;
time=_n_;
run;

proc autoreg data=spr5;
model return=/nlag=(1 2 3);
output out=out r= resi;
run;

proc arima data=out;
identify var=resi nlag=60;
run;

data out;
set out;
resi2=resi**2;
run;

proc arima data=out;
identify var=resi2 nlag=60;
run;

/* inadequate to fit garch model */