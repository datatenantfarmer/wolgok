/* prob 1 */
data prob1;
input resi $ exp $ arr $ freq @@;
cards;
n y y 42 n y n 109 n n y 17 n n n 75
y y y 33 y y n 175 y n y 53 y n n 359
;
run;

proc logistic data=prob1 descending;
freq freq;
class resi(param=ref) exp(param=ref);
model arr = resi exp;
output out=model1 prob=p;
run;

proc print data=model1;
run;

/* prob 2 */
data prob2;
input race idea candi freq @@;
cards;
1 1 1 1 1 1 2 12 1 2 1 13 1 2 2 57 1 3 1 44 1 3 2 71 1 4 1 155 1 4 2 146
1 5 1 92 1 5 2 61 1 6 1 100 1 6 2 41 1 7 1 18 1 7 2 8
2 1 1 0 2 1 2 6 2 2 1 0 2 2 2 6 2 3 1 2 2 3 2 23 2 4 1 1 2 4 2 31 2 5 1 0 2 5 2 8
2 6 1 2 2 6 2 7 2 7 1 0 2 7 2 4
;
run;

proc logistic data=prob2;
freq freq;
class race;
model candi = idea race / scale=none aggregate;
run;

/* prob 3 */
data prob3;
input lake length food freq @@;
cards;
1 1 1 23 1 1 2 4 1 1 3 2 1 1 4 2 1 1 5 8 1 2 1 7 1 2 2 0 1 2 3 1 1 2 4 3 1 2 5 5
2 1 1 5 2 1 2 11 2 1 3 1 2 1 4 0 2 1 5 3 2 2 1 13 2 2 2 8 2 2 3 6 2 2 4 1 2 2 5 0
3 1 1 5 3 1 2 11 3 1 3 2 3 1 4 1 3 1 5 5 3 2 1 8 3 2 2 7 3 2 3 6 3 2 4 3 3 2 5 5 
4 1 1 16 4 1 2 19 4 1 3 1 4 1 4 2 4 1 5 3 4 2 1 17 4 2 2 1 4 2 3 0 4 2 4 1 4 2 5 3
;
run;

data prob3;
set prob3;
if lake=1 then lake1=1; else lake1=0;
if lake=2 then lake2=1; else lake2=0;
if lake=3 then lake3=1; else lake3=0;
if length=1 then leng=0; else leng=1;
run;

proc catmod data=prob3 order=data;
weight freq;
model food = lake1 lake2 lake3 leng /pred=prob;
run;


/* prob 4 */
data prob4;
input age num freq @@;
cards;
1 1 91 1 2 90 1 3 51 2 1 150 2 2 200 2 3 155
3 1 109 3 2 198 3 3 172
;
run;

proc logistic data=prob4;
freq freq;
class age;
model num = age / scale=none aggregate;
run;

/* prob 5 */
data prob5;
input dis status event @@;
cards;
4 1 1 4 1 9 4 1 4 4 1 3 4 0 2 4 1 0 4 0 1 4 1 3 4 1 3 4 1 7 4 0 1 4 0 2
3 1 5 3 0 6 3 1 3 3 0 1 3 1 8 3 1 2 3 0 5 3 1 5 3 1 9 3 0 3 3 1 3 3 1 1 
2 0 0 2 1 4 2 0 3 2 0 9 2 1 6 2 0 4 2 0 3 1 1 8 1 1 2 1 1 7 1 0 5 1 0 4
1 0 4 1 1 8 1 0 8 1 0 9
;
run;

data prob5_sub;
set prob5;
if event < 4 then ev_new = 1;
else if event < 7 then ev_new =2;
else if event < 10 then ev_new =3;
run;

proc logistic data=prob5_sub;
class status(param=ref) ev_new;
model dis = status ev_new / scale=none aggregate;
run;


proc logistic data=prob5;
class status;
model dis = status event / scale=none aggregate;
run;

/* prob 6 */
data prob6;
input hab $ bl $ freq @@;
cards;
yes a 483 yes b 477 no a 1101 no b 1121
;
run;

proc catmod data=prob6;
weight freq;
model hab*bl = _response_ / noresponse noiter;
loglin hab bl hab*bl;
run;

proc catmod data=prob6;
weight freq;
model hab*bl = _response_ / noresponse noiter;
loglin hab bl;
run;

proc catmod data=prob6;
weight freq;
model hab*bl = _response_ / noresponse noiter;
loglin hab;
run;

/* prob 7 */
data prob7;
input seat $ car $ death $ freq @@;
cards;
y y y 659 y y n 270 y n y 532 y n n 347
n y y 432 n y n 532 n n y 269 n n n 552
;
run;

proc catmod data=prob7;
weight freq;
model seat*car*death = _response_ /noiter noresponse;
loglin seat|car seat|death car|death;
loglin seat|car seat|death death;
loglin seat|car car|death seat;
loglin seat|death car|death car;
run;

proc catmod data=prob7;
weight freq;
model seat*car*death = _response_ /noiter noresponse;
loglin seat|car seat|death car|death;
run;

proc logistic data=prob7 order=data;
class seat car;
freq freq;
model death = seat car/ aggregate scale=none;
run;

/* prob 8 */
data prob8;
input status $ lent $ com $ freq @@;
cards;
1 y y 304 1 y n 38 1 n y 92 1 n n 64 
2 y y 665 2 y n 85 2 n y 174 2 n n 113
3 y y 894 3 y n 93 3 n y 379 3 n n 321
4 y y 720 4 y n 84 4 n y 433 4 n n 297
;
run;

proc catmod data=prob8;
weight freq;
model status*lent*com = _response_ / noiter noresponse;
loglin status lent com status*lent status*com lent*com status*lent*com;
loglin status lent com status*lent status*com lent*com;
loglin status lent com status*lent lent*com;
run;

/* prob 9 */
data prob9;
input time $ gender $ type $ freq @@;
cards;
0 m i 34 0 m s 64 0 f i 49 0 f s 135
0-.5 m i 29 0-.5 m s 61 0-.5 f i 31 0-.5 f s 118
.5-1 m i 40 .5-1 m s 81 .5-1 f i 37 .5-1 f s 142
1-2 m i 37 1-2 m s 65 1-2 f i 32 1-2 f s 64
2-4 m i 24 2-4 m s 40 2-4 f i 11 2-4 f s 37
4-6 m i 6 4-6 m s 15 4-6 f i 0 4-6 f s 3
6+ m i 3 6+ m s 7 6+ f i 4 6+ f s 2
;
run;

proc catmod data=prob9;
weight freq;
model time*gender*type = _response_ / noiter noresponse;
loglin time|gender time|type type|gender;
loglin time|gender time|type;
loglin time|gender type|gender;
loglin time|type type|gender;
loglin time|gender type;
loglin type|gender time;
run;

proc catmod data=prob9;
weight freq;
model time*gender*type = _response_ / noiter noresponse;
loglin time|gender type|gender;
run;

data prob9_2;
set prob9;
if time='0' then new_t='1';
else if time='0-.5' then new_t='1';
else if time='4-6' then new_t='3';
else if time='6+' then new_t='3';
else new_t='2';
run;

proc catmod data=prob9_2;
weight freq;
model new_t*gender*type = _response_ / noiter noresponse;
loglin new_t|gender new_t|type type|gender;
loglin new_t|gender new_t|type;
loglin new_t|gender type|gender;
loglin new_t|type type|gender;
loglin new_t|gender type;
loglin new_t type|gender;
run;

proc catmod data=prob9_2;
weight freq;
model new_t*gender*type = _response_ / noiter noresponse;
loglin new_t|gender type|gender;
run;
