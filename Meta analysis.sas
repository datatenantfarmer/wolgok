/* prob 1 */
data prob1;
input p @@;
cards;
.025 .31 .009 .28 .345 .42 .06
;
run;

/* order statistic */
proc rank data=prob1 out=ord1;
var p; ranks order;

data order1;
set ord1;
k=7; alpha=0.05;
if order ^=1 then delete;
signi = 1- (1-alpha)**(1/k);
keep alpha order p signi;
label signi="critical value"; label p='p(1)';
title 'order statistics method';
proc print data=order1 label noobs; run;

/* inverse chi - square method */
data prob1_chi;
set prob1;
lnp=-2*log(p); label lnp='-2log(p)';
run;

proc means data=prob1_chi noprint;
output out=i_chi sum=sum; var lnp;
data inv_chi;
set i_chi;
chi=sum; chi_p=1-probchi(chi, 2*_freq_,0);
keep chi chi_p; label chi_p='one sided p_value'; label chi='chi-square';
title 'inverse chi-square method';
proc print data=inv_chi label noobs;
run;

/* inverse normal method */
data prob1_norm;
set prob1;
z_i=probit(p);

proc means data=prob1_norm noprint;
output out=i_nor sum=sum; var z_i;
data inv_norm; 
set i_nor;
nor=sum/sqrt(_freq_); if nor>0 then nor_p=1-probnorm(nor);
else nor_p=probnorm(nor);
nor_p2=2*nor_p; keep nor nor_p nor_p2;
label nor_p='one-sided p-value' nor_p2='two-sided p-value' nor='normal';
title 'inverse normal mdehod';
run;

proc print data=inv_norm label noobs;
run;

/* logit method */
data prob1_logit;
set prob1;
logit=log(p/(1-p));
run;

proc means data=prob1_logit noprint;
output out=prelogit sum=sum; var logit;
data logit;
set prelogit;
pi=3.141592154; l=sum*sqrt((3*(5*_freq_+4))/((pi**2)*_freq_*(5*_freq_+2)));
if l>0 then log_p=1-probt(l, 5*_freq_+4,0); else log_p=probt(l, 5*_freq_+4,0);
log_p2=2*log_p; keep l log_p log_p2;
label log_p='one-sided p-value' log_p2='two-sided p-value' l='L*';
title 'logit method';
run;

proc print data=logit label noobs; 
run;

/* prob 2 */
proc means data=prob1_norm;
var z_i;
run;

data homo;
set prob1_norm;
st_z=(z_i+1.0799709)**2;
run;

proc means data=homo;
var st_z;
output out=homo1 sum=sum;
run;

data homo_test;
set homo1;
val=1-probchi(sum, _freq_-1,0);
keep sum val;
label sum='chi-square' val='p-value';
title 'homogeneity test for p-values';
run;

proc print data=homo_test noobs label;
run;

/* prob 3 */
data prob3;
input ne nc m1 m2 s1 s2 alpha;
cards;
8 8 75.38 69.12 4.09 4.09 0.05
;
run;

data es_unb;
set prob3;
s=sqrt(((ne-1)*(s1**2)+(nc-1)*(s2**2))/(ne+nc-2));
delta1=(m1-m2)/s;
n=ne+nc; delta2=4*(n-3)/(4*n-9)*delta1;
sig=(ne+nc)/(ne*nc)+(delta2**2)/(2*n);
z=quantile('normal',1-alpha/2);
l=delta2-z*sqrt(sig); u=delta2+z*sqrt(sig);
label l='Lower 95% CI' u='Upper 95% CI' delta2='D';
keep l u delta2;
run;

proc print data=es_unb noobs label;
run;

data es_hyper;
set prob3;
s=sqrt(((ne-1)*(s1**2)+(nc-1)*(s2**2))/(ne+nc-2));
delta1=(m1-m2)/s;
n=ne+nc; delta2=4*(n-3)/(4*n-9)*delta1;
a=sqrt(4+2*(nc/ne)+2*(ne/nc));
hd=sqrt(2)*arsinh(delta2/a);
z=quantile('normal',1-alpha/2);
l1=hd-z/sqrt(n); u1=hd+z/sqrt(n);
l=a*sinh(l1/sqrt(2)); u=a*sinh(u1/sqrt(2));
keep delta2 l u;
label l="Lower 95% CI" u="Upper 95% CI" delta2="D";
run;

proc print data=es_hyper noobs label;
run;

/* prob 5 */
data prob5;
input n d @@;
cards;
90 -.581 40 .263 36 .381 20 .505 22 .275 10 .147 10 .039 10 .284 39 -.088 50 -.116
;
run;

data hd5;
set prob5;
hd=sqrt(2)*arsinh(d/sqrt(8));
hp=2*n*hd/(327*2);
run;

proc means data=hd5 sum;
var hp;
run;

data step1;
alpha=0.05;
hp=-0.0247660;
z=quantile('normal',1-alpha/2);
l1=hp-z/sqrt(327*2); u1=hp+z/sqrt(327*2);
delta=2*sqrt(2)*sinh(hp/sqrt(2));
l=2*sqrt(2)*sinh(l1/sqrt(2)); u=2*sqrt(2)*sinh(u1/sqrt(2));
keep delta l u;
label l="Lower 95% CI" u="Upper 95% CI" delta2="D";
run;

proc print data=step1 noobs label;
run;

data vst1;
set hd5;
val=2*n*(hd+0.0247660)**2;
run;

proc means data=vst1 sum;
var val;
output out = vst2 sum=sum;
run;

data vst3;
set vst2;
val=1-probchi(sum, _freq_-1,0);
keep sum val;
label sum='chi-square' val='p-value';
title 'homogeneity test for p-values';
run;

proc print data=vst3 noobs label;
run;


