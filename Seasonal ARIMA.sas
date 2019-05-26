data unemp;
infile '/home/albam10240/표5.1-우리나라의실업률자료.txt';
input unemp; time=_n_; t_unemp=-1/sqrt(unemp);
run;

proc sgplot data=unemp;
	series X=time Y=unemp;
run;

proc sgplot data=unemp;
	series X=time Y=t_unemp;
run;

proc arima data=unemp;
	identify var=t_unemp(1 12);
	estimate q=(1) (12) noconstant;
run;

/* shows acf, pacf */
proc arima data=unemp;
	identify var=t_unemp(1) nlag=24;
run;
	
data fac;
infile '/home/albam10240/표5.12-미국의신규공장설비투자액.txt';
input cost; time=_n_; l_cost=log(cost);
run;

proc sgplot data=fac;
	series X=time Y=l_cost;
run;

proc arima data=fac;
	identify var=l_cost(1 4);
	estimate p=1 q=(4 8) ml noconstant;
run;
	
	
data gas;
infile '/home/albam10240/표5.17-프로판가스의사용량자료.txt';
input gas; time=_n_; 
run;

proc sgplot data=gas;
	series X=time Y=gas;
run;

proc arima data=gas;
identify var=gas(4);
estimate p=1 q=(4) (5) noconstant;
run;



