/* prob 1 */
data prob1;
input factor dep freq @@;
cards;
1 1 5 1 2 21 2 1 8 2 2 82
;
run;

proc freq data=prob1;
tables factor*dep/measures;
weight freq;
run;

/* prob 2 */
data prob2;
input usage cancer freq @@;
cards;
1 1 273 1 2 2641 2 1 716 2 2 7260
;
run;

proc freq data=prob2;
tables usage*cancer/measures;
weight freq;
run;

/* prob 3 */
data prob3;
input group re freq @@;
cards;
1 1 49 1 2 12 2 1 24 2 2 9 3 1 2 3 2 29
;
run;

proc freq data=prob3;
weight freq;
tables group*re/chisq;
run;

/* prob 4 */
data prob4;
input hpv hiv freq @@;
cards;
1 1 23 1 2 4 1 3 10 
2 1 10 2 1 14 2 3 35
;
run;

proc freq data=prob4;
weight freq;
tables hpv*hiv/chisq;
tables hpv*hiv/fisher;
run;

/* prob 5 */
data prob5;
input method result freq @@;
cards;
1 1 21 1 2 2 2 1 15 2 2 3
;
run;

proc freq data=prob5;
weight freq;
tables method*result/fisher;
run;

/* prob 6 */
data prob6;
input group glass freq @@;
cards;
1 1 1 1 2 5 2 1 8 2 2 2
;
run;

proc freq data=prob6;
weight freq;
tables group*glass/fisher;
run;

/* prob 7 */
data prob7;
input prior post freq @@;
cards;
1 1 794 1 2 150 2 1 86 2 2 570
;
run;

proc freq data=prob7;
weight freq;
tables prior*post/agree;
run;

/* prob 8 */
data prob8;
input desease gender status freq @@;
cards;
1 1 1 2 1 1 2 21 1 2 1 0 1 2 2 10
2 1 1 2 2 1 2 40 2 2 1 0 2 2 2 18
3 1 1 6 3 1 2 33 3 2 1 0 3 2 2 10
4 1 1 17 4 1 2 16 4 2 1 0 4 2 2 4
;
run;

proc freq data=prob8;
weight freq;
tables desease*gender*status/cmh nocol norow;
run;

/* prob 9 */
data prob9;
input dia status freq @@;
cards;
1 1 302 1 2 80 2 1 179 2 2 372
;
run;

proc freq data=prob9;
weight freq;
tables dia*status;
run;