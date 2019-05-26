/* 생명표 분석법 */
data angina;
input time censor freq @@;
cards;
0.5 1 456 0.5 0 0 1.5 1 226 1.5 0 39 2.5 1 152 2.5 0 22
3.5 1 171 3.5 0 23 4.5 1 135 4.5 0 24 5.5 1 125 5.5 0 107
6.5 1 83 6.5 0 133 7.5 1 74 7.5 0 102 8.5 1 51 8.5 0 68 
9.5 1 42 9.5 0 64 10.5 1 43 10.5 0 45 11.5 1 34 11.5 0 53
12.5 1 18 12.5 0 33 13.5 1 9 13.5 0 27 14.5 0 23 15.5 1 0 15.5 0 30
;

proc lifetest method=life
intervals= 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16
plots=(S, H) graphics;
time time*censor(0);
freq freq;
run;

/* Kaplan - meier method */
data rem;
input time censor @@;
cards;
3 1 4 0 4.5 1 4.5 1 5.5 1 6 1 6.4 1 6.5 1 7 1 7.5 1
8.4 0 10 1 10 0 12 1 15 1
;
run;

proc lifetest data=rem method=km plots=(S, H) graphics;
time time*censor(0);
run; 

/* comparison of survival function */
data m1;
input time censor group @@;
cards;
33.7 0 1 3.9 1 1 10.5 1 1 5.4 1 1 19.5 1 1 23.8 0 1 7.9 1 1 
16.9 0 1 16.6 0 1 33.7 0 1 17.1 0 1 8 1 2 26.9 0 2 21.4 0 2 
18.1 0 2 16 0 2 6.9 1 2 11 0 2 24.8 0 2 23 0 2 8.3 1 2 10.8 0 2 
12.2 0 2 12.5 0 2 24.4 1 2 7.7 1 2 14.8 0 2 8.2 0 2 8.2 0 2 7.8 0 2
;
run;

proc lifetest data=m1 plots=(S, H) graphics;
time time*censor(0);
strata group;
symbol1 v=none color=black line=1;
symbol2 v=none color=black line=2;
run;

/* parametric model */
data lung;
input time censor status age post treat type @@;
cards;
411 1 70 64 5 1 1 126 1 60 63 9 1 1 118 1 70 65 11 1 1 82 1 40 69 10 1 1
8 1 40 63 58 1 1 25 0 70 48 9 1 1 11 1 70 48 11 1 1 54 1 80 63 4 1 2
153 1 60 63 14 1 2 16 1 30 53 4 1 2 56 1 80 43 12 1 2 21 1 40 55 2 1 2
287 1 60 66 25 1 2 10 1 40 67 23 1 2 8 1 20 61 19 1 3 12 1 50 63 4 1 3 
177 1 50 66 16 1 4 12 1 40 68 12 1 4 200 1 80 41 12 1 4 250 1 70 53 8 1 4
100 1 60 37 13 2 4 999 1 90 54 12 2 1 231 0 50 52 8 2 1 991 1 70 50 7 2 1
1 1 20 65 21 2 1 201 1 80 52 28 2 1 44 1 60 70 13 2 1 15 1 50 40 13 2 1 
103 0 70 36 22 2 2 2 1 40 44 36 2 2 20 1 30 54 9 2 2 51 1 30 59 87 2 2 
18 1 40 69 5 2 3 90 1 60 50 22 2 3 84 1 80 62 4 2 3 164 1 70 68 15 2 4 
19 1 30 39 4 2 4 43 1 60 49 11 2 4 340 1 80 64 10 2 4 231 1 70 67 18 2 4
;

proc lifereg data=lung;
class treat type;
model time*censor(0)=status age post treat type / dist=weibull;
run;

proc lifetest plots=(LS, LLS) graphics;
time time*censor(0);
run;

/* cox regression */
data hsv2;
input group num time censor @@;
cards;
1 12 8 1 1 10 12 0 1 7 52 0 1 10 28 1
1 6 44 1 1 8 14 1 1 8 3 1 1 9 52 0
1 11 35 1 1 13 6 1 1 7 12 1 1 13 7 0
1 9 52 0 1 12 52 0 1 13 36 1 1 8 52 0
1 10 9 1 1 16 11 0 1 6 52 0 1 14 15 1
1 13 13 1 1 13 21 1 1 16 24 0 1 13 52 0
1 9 28 1 0 9 15 1 0 10 44 0 0 12 2 0
0 7 8 1 0 7 12 1 0 7 52 0 0 7 21 1
0 11 19 1 0 16 6 1 0 16 10 1 0 6 15 0 
0 15 4 1 0 9 9 0 0 10 27 1 0 17 1 1 
0 8 12 1 0 8 20 1 0 8 32 0 0 8 15 1
0 14 5 1 0 13 35 1 0 9 28 1 0 15 6 1
;
proc phreg data=hsv2;
model time*censor(0) = num;
strata group;
baseline out=d loglogs=lls;
run;

proc gplot data=d;
plot lls*time=group;
symbol1 interpol=join line=1;
symbol2 interpol=join line=2;
run;

proc phreg data=lung;
class treat type;
model time*censor(0) = status age post treat type /selection=stepwise; 
run;

/* plot cumulative hazard function */
proc phreg data=lung;
class treat type;
model time*censor(0)= status age post treat type;
baseline out=a survival=s logsurvival=ls loglogs=lls;
proc print data=a;
data b; set a; ls=-ls;
proc gplot data=b;
plot ls*time;
symbol1 value=none interpol=join;
run;

/* proportional hazard? */
proc phreg data=lung;
class type;
model time*censor(0) = status age post type;
strata treat;
baseline out=c survival=s loglogs=lls;
proc gplot data=c;
plot lls*time=treat;
symbol1 interpol=join line=1;
symbol2 interpol=join line=2;
run;
