/* CH3. 지수평활법 */
data govern;
input con @@;
time = _n_;
cards;
4830.4 4814.5 5895.3 7151.0 5624.2 5722.2 7006.2 8153.6
6411.9 6502.5 7603.1 8732.3 7034.8 7317.0 8569.3 9935.6
7829.8 8074.0 9689.5 10840.2 9344.9 9277.6 11154.3 12700.6
10062.7 10036.6 12011.4 13549.0 11160.1 10769.2 12935.4 13927.4
11072.5 10728.6 13581.0 14707.3 11804.0 11208.3 14216.8 15587.2
12443.3 12096.7 15729.9
;
run;

proc sgplot data=govern;
series X=time Y=con;
run; 

proc forecast data=govern lead=4 out=out0 outfull trend=2
outest=est outfitstats seasons=4 method=winters;
id time; var con;
run;

proc print data=out0; run;
proc print data=est; run;

/* show prediction line, confidence level */
goptions reset=all ftext=swissx fontres=presentation;
symbol1 v=circle cv =black i=join ci=black;
symbol2 v=dot cv=black i=spline ci=black;
symbol3 v=none i=spline ci=black l=3;
symbol4 v=none i=spline ci=black l=3;
legend1 across=1 cborder=black mode=reserve cframe=white
	position=(top inside left) label=none
	value=(tick=1 font=swissx 'original series'
		   tick=2 font=swissx 'forecast series'
		   tick=3 font=swissx 'lower 95% confidence limit'
		   tick=4 font=swissx 'upper 95% confidence limit');

proc gplot data=out0; where time>=1;
 plot con*time=_type_;
 run;
 
/* 단순지수평활법 */
data trip;
input con @@;
time=_n_;
cards; 
28.9 31.1 33.9 35.8 35.2 33.1 31.0 35.7 40.3 32.0 35.0 33.2
30.9 25.6 26.1 27.2 31.2 29.5 30.1 30.7 29.9 32.3 30.2 25.7
30.4 26.5 29.1 33.4
;

proc sgplot data=trip;
	series X=time Y=con;
run;

proc forecast data=trip lead=4 out=out2 outfull trend=1
outest=est2 outfitstats seasons=12 method=expo;
id time; var con;
run;

proc print data=out2; run;
proc print data=est2; run;

goptions reset=all ftext=swissx fontres=presentation;
symbol1 v=circle cv =black i=join ci=black;
symbol2 v=dot cv=black i=spline ci=black;
symbol3 v=none i=spline ci=black l=3;
symbol4 v=none i=spline ci=black l=3;
legend1 across=1 cborder=black mode=reserve cframe=white
	position=(top inside left) label=none
	value=(tick=1 font=swissx 'original series'
		   tick=2 font=swissx 'forecast series'
		   tick=3 font=swissx 'lower 95% confidence limit'
		   tick=4 font=swissx 'upper 95% confidence limit');

proc gplot data=out2; where time>=1;
 plot con*time=_type_;
 run;
 

data seoul;
input area @@;
time=_n_;
cards;
609.81 609.81 613.00 627.86 628.01 600.39 607.97
611.24 607.15 607.28 607.27 605.26 605.33 605.32
605.38 605.43 605.42 605.42 605.40 605.43 605.34
605.33 605.36 605.40 605.43 605.78 605.58 605.52
605.52 605.52 605.50
;

proc sgplot data=seoul;
	series X=time Y=area;
run;

proc forecast data=seoul lead=6 out=out3 outfull trend=1
outest=est3 outfitstats method=expo;
id time; var area;
run;

proc print data=out3; run;
proc print data=est3; run;

goptions reset=all ftext=swissx fontres=presentation;
symbol1 v=circle cv =black i=join ci=black;
symbol2 v=dot cv=black i=spline ci=black;
symbol3 v=none i=spline ci=black l=3;
symbol4 v=none i=spline ci=black l=3;
legend1 across=1 cborder=black mode=reserve cframe=white
	position=(top inside left) label=none
	value=(tick=1 font=swissx 'original series'
		   tick=2 font=swissx 'forecast series'
		   tick=3 font=swissx 'lower 95% confidence limit'
		   tick=4 font=swissx 'upper 95% confidence limit');

proc gplot data=out3; where time >= 1;
	plot area*time=_type_;
run;

/* 이중지수평활법 */
data consumer;
infile '/home/albam10240/표3.12-전도시소비자물가지수.txt';
input month price;
time=_n_;
run;

proc sgplot data=consumer;
	series X=time Y=price;
run;

proc reg data=consumer;
model price=time/DW;
output out=con1;
run;

proc forecast data=consumer lead=12 out=con2 outfull
trend=2 outest=est2 method=expo;
id time; var price;
run;

proc print data=est2; run;
proc print data=con2; run;

goptions reset=all ftext=swissx fontres=presentation;
symbol1 v=circle cv =black i=join ci=black;
symbol2 v=dot cv=black i=spline ci=black;
symbol3 v=none i=spline ci=black l=3;
symbol4 v=none i=spline ci=black l=3;
legend1 across=1 cborder=black mode=reserve cframe=white
	position=(top inside left) label=none
	value=(tick=1 font=swissx 'original series'
		   tick=2 font=swissx 'forecast series'
		   tick=3 font=swissx 'lower 95% confidence limit'
		   tick=4 font=swissx 'upper 95% confidence limit');

proc gplot data=con2; where time >= 1;
	plot price*time=_type_;
run;

/* 계절형 지수평활법 */
data air;
infile '/home/albam10240/표3.17-서울지역대기오염(오존)자료.txt';
input month oz;
time=_n_;
run;

proc sgplot data=air;
	series X=time Y=oz;
run;

proc forecast data=air lead=12 out=air1 outfull
trend =1 outest=es method=addwinters seasons=12;
id time; var oz;
run;

proc print data=air1; run;
proc print data=es; run;

goptions reset=all ftext=swissx fontres=presentation;
symbol1 v=circle cv =black i=join ci=black;
symbol2 v=dot cv=black i=spline ci=black;
symbol3 v=none i=spline ci=black l=3;
symbol4 v=none i=spline ci=black l=3;
legend1 across=1 cborder=black mode=reserve cframe=white
	position=(top inside left) label=none
	value=(tick=1 font=swissx 'original series'
		   tick=2 font=swissx 'forecast series'
		   tick=3 font=swissx 'lower 95% confidence limit'
		   tick=4 font=swissx 'upper 95% confidence limit');

proc gplot data=air1; where time >= 1;
	plot oz*time=_type_;
run;

/* 계절형 지수평활법, 추세 */
data mart;
infile '/home/albam10240/표3.21-대형할인점판매액지수.txt';
input month sales;
time=_n_;
run;

proc sgplot data=mart;
	series X=time Y=sales;
run;

proc forecast data=mart lead=12 out=mart1 outfull trend=2
seasons=12 method=winters outest=est;
id time; var sales;
run;

proc print data=mart1; run;
proc print data=est; run;

goptions reset=all ftext=swissx fontres=presentation;
symbol1 v=circle cv =black i=join ci=black;
symbol2 v=dot cv=black i=spline ci=black;
symbol3 v=none i=spline ci=black l=3;
symbol4 v=none i=spline ci=black l=3;
legend1 across=1 cborder=black mode=reserve cframe=white
	position=(top inside left) label=none
	value=(tick=1 font=swissx 'original series'
		   tick=2 font=swissx 'forecast series'
		   tick=3 font=swissx 'lower 95% confidence limit'
		   tick=4 font=swissx 'upper 95% confidence limit');

proc gplot data=mart1; where time >= 1;
	plot sales*time=_type_;
run;
