 data retail;
  input  retail @@;
  date=intnx('month', '1jan80'd, _n_-1);
  format date mmyys7.;
  cards;
  14.7 	14.8 	15.7 	16.3 	16.1 	16.0 	15.6 	15.2 	15.3 	16.0 	16.9 	19.0 
 18.2 	17.6 	20.0 	21.0 	21.0 	20.7 	19.5 	19.9 	20.7 	20.6 	20.9 	22.1  
 19.0 	17.5 	19.4 	19.8 	20.3 	20.6 	20.3 	19.8 	22.4 	19.8 	22.8 	25.6 
 23.0 	20.9 	22.8 	23.5 	24.1 	25.0 	23.5 	22.4 	24.2 	25.0 	25.8 	28.0 
 27.8 	23.2 	25.7 	25.9 	28.0 	26.9 	26.1 	23.8 	26.4 	26.8 	27.7 	31.7 
 26.8 	26.3 	28.9 	28.6 	28.9 	28.4 	27.9 	26.5 	30.2 	29.5 	30.4 	34.5 
 31.6 	31.4 	34.3 	34.2 	33.8 	32.8 	32.1 	32.0 	36.1 	35.4 	36.7 	39.6 
 36.9 	34.5 	38.6 	38.2 	38.2 	36.4 	35.7 	34.8 	38.7 	41.7 	40.4 	44.5 
 39.6 	40.6 	41.1 	40.9 	41.0 	40.0 	39.9 	39.7 	45.5 	44.3 	45.9 	49.6 
 44.5 	45.6 	48.7 	49.0 	48.9 	48.1 	47.7 	46.2 	52.6 	52.2 	53.0 	58.1 
 56.1 	52.2 	57.9 	58.3 	57.9 	56.7 	55.7 	53.8 	59.9 	61.2 	61.1 	66.4 
 61.2 	60.8 	65.7 	66.4 	65.0 	65.4 	64.3 	61.9 	70.0 	70.5 	70.4 	75.5 
 71.8 	68.4 	72.6 	73.9 	72.3 	69.4 	70.1 	66.9 	74.8 	76.0 	75.4 	79.3 
 89.1 	73.3 	79.6 	80.8 	80.2 	78.4 	78.9 	72.9 	83.9 	85.5 	83.8 	91.1 
 87.6 	84.0 	87.6 	92.7 	87.2 	84.4 	89.0 	80.8 	92.2 	97.1 	95.1 	102.2 
 103.1 	87.4 	96.3 	105.1 	98.0 	94.1 	98.3 	89.2 	101.8 	107.2 	103.6 	115.9 
 111.3 	105.5 	108.9 	117.8 	111.0 	106.7 	112.8 	101.9 	116.0 	123.4 	116.5 	126.9 
 120.2 	113.6 	119.9 	125.5 	121.6 	115.3 	121.3 	110.2 	125.3 	129.9 	123.6 	126.5 
 118.4 	111.2 	119.6 	120.4 	118.6 	111.0 	111.5 	105.7 	115.3 	122.8 	122.0 	125.9 
 122.3 	120.5 	127.0 	128.9 	128.6 	122.5 	126.1 	119.2 	132.9 	139.3 	138.6 	147.1 
 140.2 	134.8 	141.5 	146.6 	144.6 	138.4 	140.8 	131.7 	149.0 	149.9 	148.7 	154.8 
 152.9 	137.2 	150.3 	151.9 	154.5 	147.1 	149.8 	140.6 	160.8 	159.6 	162.8 	172.3 
 ;
 run;

%origin_p(retail, 1980, 1, 1, retail);
/* increasement in variation through trend -> Multiplicative model*/

proc x11 data=retail yraheadout; /* yraheadout : 아리마로 예측치 전후 추가 */
arima forecast=1; /* 한 개의 예측치, 1개년의 예측치 도출 */
monthly date=date charts=none printout=none;
var retail;
output out=out_x11  d10=sf d11=sadjs d12=tcs d13=is;
tables a13  d10 d11 d12 d13 f2;
run;

data test;
set out_x11;
t=tcs*is;
o=sf*sadjs;
oo=sf*tcs*is;
run;

%origin_p(test, 1980, 1,1, oo);
%origin_p(retail, 1980, 1, 1, retail);

%origin_p(test, 1980, 1, 1, t);
%origin_p(out_x11, 1980, 1, 1, sadjs);

%origin_p(test, 1980, 1,1, o);
%origin_p(retail, 1980, 1, 1, retail);


/* final seasonal factor */
%origin_p(out_x11, 1980, 1, 1, sf);
/* seasonal adjusmented series */
%origin_p(out_x11, 1980, 1, 1, sadjs);
/* final trend-cycle series */
%origin_p(out_x11, 1980, 1, 1, tcs);
/* final irregular series */
%origin_p(out_x11, 1980, 1, 1, is);

/* multiplicative X-11 arima */
data drink;
infile '/home/albam10240/표7.1-청량음료판매량자료.txt';
input drink;
date=intnx('month', '1jan80'd, _n_-1);
format date mmyys.;
run;

%origin_p(drink, 2005, 1, 1, drink);

proc x11 data=drink yraheadout;
arima forecast=1;
monthly date=date charts=none printout=none;
var drink;
output out=x11_out  d10=sf d11=sadjs d12=tcs d13=is;
tables a13  d10 d11 d12 d13 f2;
run;

%origin_p(x11_out, 1980, 1, 1, sf);
/* seasonal adjusmented series */
%origin_p(x11_out, 1980, 1, 1, sadjs);
/* final trend-cycle series */
%origin_p(x11_out, 1980, 1, 1, tcs);
/* final irregular series */
%origin_p(x11_out, 1980, 1, 1, is);

data oo;
set x11_out;
fore=sadjs*sf;
run;

%origin_p(oo, 2005, 1,1, fore);
%origin_p(drink, 2005, 1, 1, drink);
