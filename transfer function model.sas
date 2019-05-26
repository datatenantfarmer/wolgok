data trans1;
infile '/home/albam10240/표9.1-우리나라수출신용장내도액자료.txt';
input lc @@;
run;

data trans2;
infile '/home/albam10240/표9.2-우리나라수출액자료.txt';
input ex @@;
run;

%origin_p(trans1,1990,1,1,lc);
%origin_p(trans2,1990,1,1,ex);

data trans; merge trans1 trans2;
rex=sqrt(ex); rex1=dif(rex); rex112=dif12(rex1);
rlc=sqrt(lc); rlc1=dif(rlc); rlc112=dif12(rlc1);
time=_n_;
run;

%origin_p(trans,1990,1,1,rex);
%origin_p(trans,1990,1,1,rex1);
%origin_p(trans,1990,1,1,rex112);
%origin_p(trans,1990,1,1,rlc);
%origin_p(trans,1990,1,1,rlc1);
%origin_p(trans,1990,1,1,rlc112);


proc arima data=trans;
identify var=rex(1 12) nlag=36;
estimate p=1 q=(2 6 7)(12) noconstant;
forecast lead=2;
run;

proc arima data=trans;
identify var=rlc(1 12) nlag=48;
estimate p=(1 2 3 4 5 6 7)(12) q=(1 6)(12);
estimate p=(3 4 7) q=(1 6)(12) noconstant;
identify var=rex(1 12) crosscorr=(rlc(1 12)) nlag=24;
estimate input= ((10 11 14 15 21)/(1)rlc);
estimate p=(3 4 9) q=(7 9)(12) input=((11 15)/(1)rlc) noconstant;
forecast lead=2 out=f_trans1;
run;

data f_trans;
set f_trans1;
forecast=forecast**2; R=ex-forecast;
u95=u95**2; l95=l95**2; run; 

%fore_p(f_trans, 1990, 1,1, ex)






