data stock;
infile '/home/albam10240/표4.3-한신공영주식의일일종가자료.txt';
input price;
time=_n_;
run;

proc sgplot data=stock;
	series X=time Y=price;
run;

proc arima data=stock;
identify var=price;
identify var=price(1);
estimate q=2 plot noconstant;
run;

data sea;
infile '/home/albam10240/표4.11-군산해안용존산소량자료.txt';
input oz;
time=_n_;
soz=sqrt(oz);
run;

proc sgplot data=sea;
	series X=time Y=soz;
run;

proc arima data=sea;
identify var=soz;
estimate p=(2) q=(2 4);
run;

