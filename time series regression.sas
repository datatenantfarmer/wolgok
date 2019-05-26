/* 시계열 회귀*/
data agri;
 input time pop @@;
    t1=time;  t2=time*time;  t3=time*time*time;
 cards;
1 14422 2 13244 3 10827 4 8521 5 6661 6 6068 7 5707 8 5407
9 5167 10 4851 11 4692 12 4468 13 4400 14 4210 15 4031
16 .
17 .
;
run;

proc reg data=agri ;
 model pop= t1 t2 t3 /DW;
 output out=agri2 Pred=pop_P U95=pop_u L95=pop_L residual=resid; 
run;

proc sgplot data=agri2;
	series X=time Y=pop;
	series X=time Y=pop_p;
	series X=time Y=pop_u;
	series X=time Y=pop_L;
run;

proc sgplot data=agri2;
	series X=time Y=resid;
run;

/* 오차시계열 + 1차 자기상관 */
data autoc;
 et_p=0; et_n=0;
 do i=1 to 50;
  a = rannor(1234567);
  et_p= 0.9*et_p+a;
  et_n=-0.7*et_n+a;
  output;
 end;
run;

 
symbol1 v=dot cv=black i=join ci=black;
symbol2 v=circle cv=black i=join ci=black;
proc gplot data=autoc;
 plot et_p*i=1  / vref=0 grid frame;
 plot et_n*i=2  / vref=0 grid frame;
run;

/* time series regression with seasonal effect */
data gas;
 input time gas @@; 
  D1=0; D2=0; D3=0; D4=0; D5=0; D6=0; 
  D7=0; D8=0; D9=0; D10=0; D11=0;
  array dum(11) D1-D11;
   do i=1 to 11;
    if mod(time,12) = i then dum[i]=1;
   end;
   trigo1=sin(2*3.141592*time/12);
   trigo2=time*sin(2*3.141592*time/12);
   trigo3=cos(2*3.141592*time/12);
   trigo4=time*cos(2*3.141592*time/12);
   sgas=sqrt(gas);
   lgas=log(gas);
 cards; 
 1 8291 2 7949 3 6739 4 5859 5 4420 6 2795 7 2002 8 2539 9 3307 10 5726 
11 7447 12 13976 13 19998 14 19937 15 16373 16 14168 17 8513  18 5442 
19 5096 20 4259 21 4747 22 8712 23 15619 24 19921 25 23461 26 22630
27 17671 28 14775 29 9714 30 7127 31 6404 32 6080 33 7422 
34 11241 35 17544 36 20611 37 22839 38 22680 39 16950
40 13209 41 8207 42 7291 43 7093 44 6298 45 6946 46 10332
47 19017 48 25594 49 27981 50 25455 51 22433 52 16817 53 11755 54 8709 55 8303 
56 . 57 . 58 . 59 . 60 . 61 . 62 . 63 . 64 . 65 .
;
run;

symbol v=circle cv=black i=join;
proc gplot data=gas;
 where gas ne .;
 plot gas*time=1 /grid frame;
run; 

/* dummy variable to reflect seasonal effect */
proc reg data=gas;
model gas = time D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 /DW;
output out=gas2 pred=gas_p U95=gas_u L95=gas_l residual=resid;
run;

proc sgplot data=gas2;
	series X=time Y=gas;
	series X=time Y=gas_p;
	series X=time Y=gas_u;
	series X=time Y=gas_l;
run;

/* 삼각함수 시계열 회귀분석 */
proc reg data=gas;
model gas = time trigo1 trigo2 trigo3 trigo4 / DW;
output out=gas3 pred = gas_p u95=gas_u l95=gas_l residual=resid;
run;

proc sgplot data=gas2;
	series X=time Y=gas;
	series X=time Y=gas_p;
	series X=time Y=gas_u;
	series X=time Y=gas_l;
run;