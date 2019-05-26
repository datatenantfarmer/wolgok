/* prob 2 */
data prob2;
input m1 m2 m3 m4 treat $ @@;
cards;
30 28 16 34 a 14 18 10 22 a 24 20 18 30 a 38 34 20 44 a 26 28 14 30 a
;
run;

proc glm data=prob2;
class treat;
model m1 m2 m3 m4 = treat / nouni ss3;
repeated month 4 (1 2 3 4) profile/summary printe;
run;

/* prob 3 */
data prob3; 
input gender $ A10 A20 A30 A40 B10 B20 B30 B40 @@; 
cards; 
m 21.3 21.1 22.6 24.2 21.1 22.1 31.3 25.2 m 22.1 22.6 25.2 26.8 22.6 26.2 30.5 26.2 
m 21.6 25.2 25.7 28.2 34.1 31.9 27.9 25.9 m 24.7 25.7 26.2 27.9 28.2 35.6 26.5 30.2 
m 22.6 24.1 23.8 24.5 29.2 29.6 30.5 33.1 f 27.3 20.1 20.1 24.1 28.9 29.5 26.3 23.7 
f 22.6 26.5 30.4 26.8 29.2 28.5 25.9 26.2 f 24.2 23.6 24.2 27.2 30.2 30.1 33.2 25.2 
f 26.7 23.7 25.2 27.5 27.6 26.5 31.0 22.6 f 22.3 28.2 28.1 24.7 25.6 29.3 29.9 32.9 
; 
run; 

proc glm data=prob3; 
class gender; 
model A10 -- B40 = gender / ss3 nouni; 
repeated time 4 (1 2 3 4), method 2 (1 2) contrast(1)/summary printe; 
run;

/* prob 4 */
data prob4;
input med $ m1 m2 m3 @@;
cards;
new 6 3 0 new 7 3 1 new 4 1 2 new 8 3 3
old 9 5 5 old 9 4 6 old 5 3 4 old 6 2 3
;
run;

proc glm data=prob4;
class med;
model m1 m2 m3 = med /ss3;
repeated month 3 (1 2 3) contrast(1)/summary printe;
run;

/* prob 5 */
data prob5;
input treat $ a1b1 a1b2 a1b3 a1b4 a2b1 a2b2 a2b3 a2b4 a3b1 a3b2 a3b3 a3b4 @@;
cards;
a 23 9 6 18 8 11 17 13 3 29 15 4 a 5 30 6 26 29 12 28 2 19 15 11 23 a 26 5 21 30 25 23 12 24 2 14 18 20
b 11 40 24 35 12 22 17 38 33 29 34 32 b 14 11 22 20 21 27 18 34 16 29 33 35 b 39 19 31 27 23 35 17 22 21 10 24 16
;
run;

proc glm data=prob5;
class treat;
model a1b1 -- a3b4= treat /nouni ss3;
repeated factor1 3 (1 2 3), factor2 4 (1 2 3 4) /printe;
run;

/* prob 6 */
data prob6;
input treat $ t0 t1 t2 t3 t4 @@;
cards;
n 190 212 213 195 248 n 98 137 185 215 225 n 155 145 196 189 176 n 245 228 280 274 260
n 182 205 218 194 193 n 140 138 187 195 205 n 196 185 185 227 180 n 162 176 192 230 215
n 195 232 199 185 200 n 167 187 228 192 210 n 123 165 145 185 215 n 105 144 119 168 165
n 161 177 162 185 192 n 255 242 330 284 319 n 180 218 224 165 200 n 144 195 180 184 213
n 126 145 173 175 140 n 175 155 154 164 154 n 227 218 245 235 257
p 187 177 200 190 206 p 205 230 172 196 232 p 165 142 195 185 170 p 256 232 252 326 292
p 197 182 160 210 185 p 134 115 150 165 170 p 196 166 166 188 205 p 167 144 176 155 185
p 98 102 89 128 130 p 167 175 122 162 125 p 123 136 147 130 135 p 95 102 154 105 112
p 181 177 140 212 230 p 237 232 245 193 245 p 144 172 163 158 188 p 182 202 254 185 173
p 165 140 153 180 155 p 196 195 204 188 178 p 175 197 195 182 193
;
run;

proc glm data=prob6;
class treat;
model t0 t1 t2 t3 t4 = treat/ nouni ss3;
repeated time 5 (0 1 2 3 4) / printe;
run;