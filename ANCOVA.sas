/* prob 1 */
data prob1;
input treat $ x y @@;
cards;
a 5 20 a 10 23 a 12 30 a 9 25 a 23 34 a 21 40 a 14 27 a 18 38 a 6 24 a 13 31
b 7 19 b 12 26 b 27 33 b 24 35 b 18 30 b 22 31 b 26 34 b 21 28 b 14 23 b 9 22
;
run;

proc glm data=prob1;
class treat;
model y = x treat /solution;
lsmeans treat / tdiff;
run;

/* prob 2 */
data prob2;
input method $ x y @@;
cards;
1 29 39 1 4 34 1 18 36 2 17 35 2 35 38 2 3 32 3 1 38 3 15 43 3 32 44
;
run;

proc glm data=prob2;
class method;
model y = method x/ solution;
lsmeans method / tdiff;
run;

/* prob 3 */
data prob3;
input med $ x y @@;
cards;
a 11 6 a 8 0 a 5 2 a 14 8 a 19 11 a 6 4 a 10 13 a 6 1 a 11 8 a 3 0
b 6 0 b 6 2 b 7 3 b 8 1 b 18 18 b 8 4 b 19 14 b 8 9 b 5 1 b 15 9
c 16 13 c 13 10 c 11 18 c 9 5 c 21 23 c 16 12 c 12 5 c 12 16 c 7 1 c 12 20
;
run;

proc glm data=prob3;
class med;
model y = med x med*x/ solution;
lsmeans med / tdiff;
run; 

proc glm data=prob3;
class med;
model y = med x/ solution;
lsmeans med / tdiff;
output out=r residual =r p=p;
run; 

proc univariate data=r;
var r;
probplot r;
run;

proc sgplot data=r;
scatter x=p y=r;
run;

/* prob 4 */
data prob4;
input pro $ x y @@;
cards;
1 38 21 1 39 26 1 36 22 1 45 28 1 33 19
2 43 34 2 38 26 2 38 29 2 27 18 2 34 25
3 24 23 3 32 29 3 31 30 3 21 16 3 28 29
;
run;

proc glm data=prob4;
class pro;
model y = x pro x*pro/ solution;
lsmeans pro / tdiff;
run;


/* prob 5 */
data prob5;
input tem $ water fer growth @@;
cards;
1 23.5 15 5 1 24 15.5 4.5 1 23 14 5 1 24.5 13.5 4.5 1 22.5 13.5 5.5 1 24.5 13.5 3 
1 25.5 13 5 1 25 14 5 1 23 14.5 5.5 1 23 14 4.5 1 22.5 14 6.5 1 23 15 5.5
2 28.5 16 7 2 26 15.5 9 2 28.5 17 8.5 2 26.5 16 8.5 2 27 18.5 6 2 28 14.5 8 2 26.5 17.5 7
2 29 13.5 7 2 27 16.5 7 2 27 16.5 11 2 27 17.5 9 2 27 18.5 8
3 25.6 12 6.6 3 23.1 11.6 9.2 3 20.8 18.2 5.6 3 19.2 11.9 4.2 3 21.3 20.2 4.6 3 26.6 13.1 4.5
3 25.5 16.2 4.5 3 18.9 11.9 6.1 3 29.3 10.5 6.3 3 30.2 11.3 8.1 3 18.9 10.9 4.9 3 25.3 12.2 6.2
;
run;

proc glm data=prob5;
class tem;
model growth = water fer tem water*tem fer*tem / solution;
run;

proc glm data=prob5;
class tem;
model growth = water fer tem water*tem / solution;
run;


proc glm data=prob5;
class tem;
model growth = water fer tem / solution;
lsmeans tem / adjust=scheffe st;
output out=r p=p residual=r;
run;

proc univariate data=r;
var r;
probplot r;
run;

proc sgplot data=r;
scatter x=p y=r;
run;

/* prob 6 */
data prob6;
input type $ wt lim @@;
cards;
a 3 12.9 a 2.8 12.5 a 3.6 12.7 a 6.7 13.1 a 7.4 12.1 a 4.9 12.7 a 5.7 12.5 a 3 11.3 a 4.7 12.5
a 4.2 11.3 a 5.3 13.1 a 7 13.1 b 8 2.3 b 7.2 2.2 b 7.6 2.5 b 9 3 b 2.3 0.6 b 8.7 3 b 8 2.6 
b 7.2 2.5 b 4.6 1.7 b 6.8 1.7 b 3.5 1.5 b 2.4 1 c 8.4 1 c 8 0.5 c 7.4 0.9 c 8.9 1.9 c 5.6 2.1
c 8 1 c 7.6 1 c 5.4 0.7 c 6.9 1.5 c 4.5 1.2 c 9.1 1.2 c 9 1.7
;
run;


proc glm data=prob6;
class type;
model lim = wt type / solution;
lsmeans type / tdiff;
run;

proc glm data=prob6;
class type;
model lim = wt type wt*type / solution;
lsmeans type / tdiff;
run;

proc glm data=prob6;
class type;
model lim = wt type wt*type /solution;
estimate 'wt:type a' wt 1 wt*type 1 0 0;
estimate 'wt:type b' wt 1 wt*type 0 1 0;
estimate 'wt:type c' wt 1 wt*type 0 0 1;
run;

/* prob 7 */
data prob7;
input method $ int mot ach @@;
cards;
1 99 12 58 1 95 11 55 1 99 13 59 1 99 13 55 1 102 12 60 1 101 12 57 1 102 12 57
1 103 13 61 1 102 13 58 1 103 14 59 1 102 14 62 1 104 13 59 1 106 13 62 
1 106 15 60 1 107 13 60 1 108 12 62 1 109 13 64 1 111 13 63 1 110 13 59
1 113 13 63 2 95 12 58 2 97 12 55 2 100 13 60 2 100 13 55 2 102 13 60 2 105 12 58 
2 102 13 57 2 102 14 62 2 105 12 58 2 105 13 61 2 106 14 63 2 109 13 59 2 107 14 61
2 108 14 64 2 111 14 61 2 108 13 63 2 111 13 65 2 113 14 64 2 114 13 62 2 114 14 60
3 94 12 59 3 95 12 56 3 99 13 62 3 99 11 57 3 99 14 61 3 102 12 60 3 99 13 58 3 101 13 63
3 104 13 60 3 101 14 63 3 104 15 65 3 107 13 61 3 107 13 62 3 106 14 64 3 109 14 61
3 107 14 65 3 109 14 63 3 111 14 66 3 111 14 62 3 112 13 64
;
run;

proc glm data=prob7;
class method;
model ach = method int mot int*method mot*method / solution;
estimate '1' mot 1 mot*method 1 0 0;
estimate '2' mot 1 mot*method 0 1 0;
estimate '3' mot 1 mot*method 0 0 1;
run;

proc glm data=prob7;
class method;
model ach = method int mot mot*method / solution;
run;


proc glm data=prob7;
class method;
model ach = method int mot / solution;
lsmeans method / tdiff ;
output out = r residual=r p=p;
run;

proc univariate data=r;
probplot r;
run;

proc sgplot data=r;
scatter x=p y=r;
run;