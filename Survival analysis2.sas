/* prob 2 */
data prob2;
input time censor freq @@;
cards;
0.5 1 82 0.5 0 0 1.5 1 30 1.5 0 8 2.5 1 27 2.5 0 8
3.5 1 22 3.5 0 7 4.5 1 26 4.5 0 7 5.5 1 25 5.5 0 28
6.5 1 20 6.5 0 31 7.5 1 11 7.5 0 32 8.5 1 14 8.5 0 24
9.5 1 13 9.5 0 27 10.5 1 5 10.5 0 22 11.5 1 5 11.5 0 23
12.5 1 5 12.5 0 18 13.5 1 2 13.5 0 9 14.5 1 3 14.5 0 7
15.5 1 3 15.5 0 11
;
run;

proc lifetest data=prob2 method=life
plots=(S, H) graphics
intervals= 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15;
time time*censor(0);
freq freq;
run;

/* prob 3 */
data prob3;
input time censor @@;
cards;
2 1 4 1 5 1 10 1 10 0 12 1 12 0 14 1 14 1 15 1 16 1 18 1 19 1 23 1
25 1 26 0 27 1 30 0 31 1 34 1 35 1 37 0 38 1 39 1 42 0 43 0 46 1 
47 0 49 1 50 1 53 0 54 0
;
run;

proc lifetest data=prob3 method=km;
time time*censor(0);
run;

/* prob 4 */
data prob4;
input group $ time censor @@;
cards;
a 1 1 a 2 1 a 5 1 a 5 1 a 5 1 a 7 1 a 9 1 a 11 1 a 11 1 a 13 1 a 13 1 a 16 1 a 20 1
a 21 1 a 22 0 a 22 1 a 31 0 a 33 0 a 37 0 a 43 1 b 1 1 b 3 1 b 4 1 b 4 1 b 5 1 b 7 1 b 7 1 b 9 1
b 9 1 b 14 0 b 17 1 b 19 0 b 27 0 b 30 0 b 41 0
;
run;

proc lifetest data=prob4 plots=(S, H) graphics;
time time*censor(0);
strata group;
run; /* wilcoxon value : Gehan's statistic */


/* prob 5 */
data prob5;
input censor time age bmi dage smok @@;
cards;
1 3.6 63 25.1 46 1 1 15.4 71 26 59 0 1 11.3 51 32 49 1 
1 10.3 59 28.1 57 1 1 5.8 50 26.1 49 1 0 8 66 45.3 49 0
1 14.6 42 30 41 1 1 11.4 40 35.7 36 1 1 7.2 67 28.1 61 0 
1 5.5 86 32.9 61 0 1 11.1 52 37.6 46 1 1 16.5 42 43.4 37 0
1 10.9 60 25.4 60 0 1 2.5 75 49.7 57 1 0 10.8 81 35.2 81 0
1 4.7 60 37.3 39 0 0 5.5 60 26 42 0 1 4.5 63 21.8 60 1
1 9 62 18.2 43 0 1 6.8 57 34.1 41 1 0 3.6 71 25.6 54 1
1 12.1 58 35.1 45 0 1 8.1 42 32.5 28 1 1 11.1 45 44.1 40 0
0 7 66 29.7 59 1 1 1.5 61 29.2 54 0 1 11.7 48 25.2 30 1
1 0.3 82 25.3 50 0
;
run;

proc lifetest data=prob5 plots=(LS, LLS) method=km outsurv=a;
time time*censor(0);
run;

data b; set a;
s=survival;
logit=log((1-s)/s);
lnorm=probit(1-s);
lt=log(time);
lls=log(-log(s));
ls=-log(s);
run;

proc gplot data=b;
plot ls*time logit*lt lnorm*lt lls*lt;
symbol interpol=join;
run;

proc lifereg data=prob5;
model time*censor(0) = age bmi age dage smok / dist=exponential;
run;

proc lifereg data=prob5;
model time*censor(0) = age bmi age dage smok / dist=weibull;
run;

proc lifereg data=prob5;
model time*censor(0) = age bmi age dage smok / dist=gamma;
run;

proc lifereg data=prob5;
model time*censor(0) = age bmi age dage smok / dist=lnormal;
run;

proc lifereg data=prob5;
model time*censor(0) = age bmi age dage smok / dist=llogistic;
run;

/* prob 6 */
data prob6;
input time censor age stat @@;
cards;
18 1 0 0 9 1 0 1 28 0 0 0 31 1 0 1 39 0 0 1 19 0 0 1 45 0 0 1 6 1 0 1
8 1 0 1 15 1 0 1 23 1 0 0 28 0 0 0 7 1 0 1 12 1 1 0 9 1 1 0 8 1 1 0
2 1 1 1 26 0 1 0 10 1 1 1 4 1 1 0 3 1 1 0 4 1 1 0 18 1 1 1 8 1 1 1 
3 1 1 1 14 1 1 1 3 1 1 0 13 1 1 1 13 1 1 1 35 0 1 0
;
run;

proc phreg data=prob6;
model time*censor(0)= age stat;
run;

proc phreg data=prob6;
model time*censor(0) =;
strata age;
baseline out=a survival=s loglogs=lls;
run;

proc gplot data=a;
plot lls*time=age;
symbol1 interpol=join line=1;
symbol2 interpol=join line=2;
run;

/* prob 7 */
data heart;
input censor time age wt stat @@;
cards;
1 28 20191 18 0 1 1032 15831 8 0 1 51 15627 12 0 1 733 21315 3 0
1 219 18920 83 0 0 1800 12135 25 0  0 1401 11153 . 0 1 263 3209 . 0
1 100 17870 46 0 1 66 22463 19 0 1 0 15147 5 5 1 0 18452 53 2
0 1408 17708 41 1 0 1322 16547 58 1 1 3 13310 . 0 1 2 15849 . 1
1 40 15552 . 1 1 45 13216 1 0 1 996 17755 2 1 1 72 17203 21 0 
1 9 20467 . 0 0 1142 13388 36 1 1 980 16760 831 . 1 285 17800 32 0
1 102 15066 . 0 1 188 17292 41 0 1 3 174560 . . 1 61 19159 10 0
0 942 14151 67 0 1 149 15072 . 0 1 343 17538 21 1 0 916 15115 78 1
1 68 17917 3 0 1 2 19199 . 0 1 69 14374 . 0 0 842 11929 27 0
1 584 17830 33 1 1 78 18735 12 0 1 32 19436 . 0 1 285 7141 57 0
0 470 19856 31 0 1 207 18645 139 0 1 340 17384 . 0 0 340 16430 310 0
0 265 17441 28 0 1 165 16013 4 1 1 16 14713 2 0 0 180 9734 13 0 
0 131 8626 21 0
;
run;

data heart;
set heart;
if wt >time or wt=. then trans=0; else trans=1;
if stat=0 or stat=. then status=0; else status=1;
run;

proc phreg data=heart;
model time*censor(0) = trans status age;
run;