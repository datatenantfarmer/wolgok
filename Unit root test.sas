 proc iml;
      create test  var{y1 y2 y3 y4};
      phi1={1 -0.75};   phi2={1 -0.92};  phi3={1 -0.80};  phi4={1 -0.92};
      theta={1 0};
      y1=armasim(phi1, theta, 5,1, 48, 1234321);
      y2=armasim(phi2, theta, 5,1, 48, 1234321);
	  y3=armasim(phi3, theta, 5,1, 48, 1234321);
	  y4=armasim(phi4, theta, 5,1, 48, 1234321);
	  do i=1 to 48;
	   y3[i,1]=y3[i,1]+0.3*i;
	   y4[i,1]=y4[i,1]+0.3*i;
     end;
	  append;
  quit;

  proc print data=test; run;

  %origin_p(test, 1995, 1, 1, y1)
  %origin_p(test, 1995, 1, 1, y2) 
  %origin_p(test, 1995, 1, 1, y3)
  %origin_p(test, 1995, 1, 1, y4)

/* AR=0 : DF test */
/* AR = p : 차분 후 오차항에 가정하는 모형 -> P에 대한 결정이 필요함 */
/* dlag : 단위근 검정을 하려는 시계열의 시차 -> 계절 단위근 검정이 가능 */
/* trend 1 : 차분 X(추세없음) -> 절편만 존재 */
/* trend 2 : 차분 1회(선형추세) -> 절편 + 기울기 */
 %dftest(test, y1, dlag=1, trend=1, AR=0, outstat=testout1)
     proc print data=testout1; run;
 %dftest(test, y2, dlag=1, trend=1, AR=0, outstat=testout2)
     proc print data=testout2; run;
 %dftest(test, y3, dlag=1, trend=2, AR=0, outstat=testout3)
     proc print data=testout3; run;
 %dftest(test, y4, dlag=1, trend=2, AR=0, outstat=testout4)
     proc print data=testout4; run;
     
/* 한신공영주식 */
data stock;
infile '/home/albam10240/표4.3-한신공영주식의일일종가자료.txt';
input stock;
run;

%origin_p(stock, 2005, 1, 1, stock);

%dftest(stock, stock, dlag=1, trend=1, AR=0, outstat=test1);
proc print data=test1;
%dftest(stock, stock, dlag=1, trend=1, AR=1, outstat=test2);
proc print data=test2;
%dftest(stock, stock, dlag=1, trend=1, AR=2, outstat=test3);
proc print data=test3;
%dftest(stock, stock, dlag=1, trend=1, AR=3, outstat=test4);
proc print data=test4;
/* 단위근이 존재 */

%dftest(stock, stock, dif=(1),dlag=1, trend=1, AR=0, outstat=test1);
proc print data=test1;
%dftest(stock, stock, dif=(1),dlag=1, trend=1, AR=1, outstat=test2);
proc print data=test2;
%dftest(stock, stock, dif=(1),dlag=1, trend=1, AR=2, outstat=test3);
proc print data=test3;
%dftest(stock, stock, dif=(1),dlag=1, trend=1, AR=3, outstat=test4);
proc print data=test4;
/* 비계절 차분 1회 후 단위근이 존재하지 않음-> 비계절 차분 1회 후 분석 시작 */
