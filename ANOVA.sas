/* prob 1 */
data fer;
input group height @@;
cards;
1 48.2 1 54.6 1 58.3 1 47.8 1 51.4 1 52 1 55.2 1 49.1 1 49.9 1 52.6
2 52,3 2 57.4 2 55.6 2 53.2 2 61.3 2 58 2 59.8 2 54.8 2 51.2 2 46.2
;
run;

proc ttest data=fer;
class group;
var height;
run;

/* prob 2 : paired????*/ 
data prob2;
input group del @@;
cards;
1 9 1 9.4 1 4.7 1 4.8 1 8.9 1 4.9 1 8.4 1 5.9 1 6.3 1 5.7
1 5 1 3.5 1 7.8 1 10.4 1 8 1 8 1 8.6 1 7 1 6.8 1 7.1
1 5.7 1 7.6 1 6.2 1 7.1 1 7.4 1 8.7 1 4.9 1 7.4 1 6.4 1 7.1
1 6.3 1 8.8 1 8.8 1 5.2 1 7.1 1 5.3 1 4.7 1 8.4 1 6.4 1 8.3
2 12.6 2 14.6 2 16.2 2 23.9 2 23.3 2 17.1 2 20 2 21 2 19.1 2 19.4
2 16.7 2 15.9 2 15.8 2 16 2 17.9 2 13.4 2 19.1 2 16.6 2 18.9 2 18.7
2 20 2 17.8 2 13.9 2 22.1 2 13.9 2 18.3 2 22.8 2 13 2 17.9 2 12.5
2 17.7 2 15.1 2 16.9 2 16.4 2 22.8 2 19.4 2 19.6 2 18.4 2 18.2 2 20.7
;
run;

proc ttest data=prob2;
class group;
var del;
run;

/* prob 3 */
data prob3;
input prior post @@;
cards;
.4 .4 .4 .5 .4 .5 .4 .9 .5 .5 .5 .5 .5 .5 .5 .5 .5 .5
.6 .6 .7 1.1 .7 1.2 .8 .8 .9 1.2 .9 1.9 1 .9 1 2 2 3.7
;
run;

proc ttest data=prob3;
paired prior*post;
run;

/* prob 4 */
data prob4;
input prior post @@;
cards;
104 108 116 118 84 89 77 71 61 66 84 83 81 88 72 76 61 68 97 96 84 81
;
run;

proc ttest data=prob4;
paired prior*post;
run;

/* prob 5 */
data prob5;
input group ac @@;
cards;
1 22.2 1 97.8 1 29.1 1 37 1 35.8 1 44.2 1 82 1 56 1 9.3 1 19.9 1 39.5 1 12.8
2 15.1 2 23.2 2 10.5 2 13.9 2 9.7 2 19 2 19.8 2 9.1 2 30.1 2 15.5 2 10.3 2 11
3 10.2 3 11.3 3 11.4 3 5.3 3 14.5 3 11 3 13.6 3 33.4 3 25 3 27 3 36.3 3 17.7
;
run;

proc anova data=prob5;
class group;
model ac=group;
run;

/* prob 6 */
data prob6;
input group score @@;
cards;
1 35 1 120 1 90 1 109 1 82 1 40 1 68 1 84 1 124 1 7 1 140
2 62 2 73 2 60 2 77 2 52 2 115 2 82 2 52 2 105 2 143 2 80
3 96 3 107 3 63 3 134 3 140 3 103 3 158 3 131 3 76 3 69 3 69
;
run;

proc anova data=prob6;
class group;
model score=group;
run;

/* prob 7 */
data prob7;
input ex diet weight @@;
cards;
1 1 7 1 2 5.3 1 3 4.9 1 4 8.8
2 1 9.9 2 2 5.7 2 3 7.6 2 4 8.9
3 1 8.5 3 2 4.7 3 3 5.5 3 4 8.1
4 1 5.1 4 2 3.5 4 3 2.8 4 4 3.3
5 1 10.3 5 2 7.7 5 3 8.4 5 4 9.1
;
run;

proc anova data=prob7;
class ex diet;
model weight = ex diet;
means ex/ scheffe;
means diet/ scheffe;
run; 

/* prob 8 */
data prob8;
input level method score @@;
cards;
1 1 58 1 2 68 1 3 60 1 4 68 1 5 64
2 1 62 2 2 70 2 3 65 2 4 80 2 5 69
3 1 67 3 2 78 3 3 68 3 4 81 3 5 70
;
run;

proc anova data=prob8;
class level method;
model score=level method;
means method/tukey;
means level/tukey;
run;

/* prob 9 */
data prob9;
input age usage score @@;
cards;
1 1 25 1 1 28 1 1 22 1 2 18 1 2 23 1 2 19 1 3 17 1 3 24 1 3 19
2 1 28 2 1 32 2 1 30 2 2 16 2 2 24 2 2 20 2 3 28 2 3 22 2 3 20
3 1 25 3 1 35 3 1 30 3 2 14 3 2 16 3 2 15 3 3 10 3 3 8 3 3 12
;
run;

proc anova data=prob9;
class age usage;
model score = age usage age*usage;
run;

/* prob 10 */
data prob10;
input gender species nut @@;
cards;
1 1 21.5 1 1 19.6 1 1 20.9 1 1 22.8 1 2 14.5 1 2 17.4 1 2 15 1 2 17.8 1 3 16 1 3 20.3 1 3 18.5 1 3 19.3
2 1 14.8 2 1 15.6 2 1 13.5 2 1 16.4 2 2 12.1 2 2 11.4 2 2 12.7 2 2 14.5 2 3 14.4 2 3 14.7 2 3 13.8 2 3 12
;
run;

proc anova data=prob10;
class gender species;
model nut = gender species gender*species;
run;