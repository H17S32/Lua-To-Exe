local r=string.byte;local d=string.char;local c=string.sub;local K=table.concat;local s=math.ldexp;local b=getfenv or function()return _ENV end;local C=setmetatable;local u=select;local i=unpack;local h=tonumber;local function _(f)local e,n,t="","",{}local a=256;local o={}for l=0,a-1 do o[l]=d(l)end;local l=1;local function i()local e=h(c(f,l,l),36)l=l+1;local n=h(c(f,l,l+e-1),36)l=l+e;return n end;e=d(i())t[1]=e;while l<#f do local l=i()if o[l]then n=o[l]else n=e..c(e,1,1)end;o[a]=e..c(n,1,1)t[#t+1],e,a=n,n,a+1 end;return table.concat(t)end;local a=_('25C25D25C27627727625F27725F25E27724V24Z25F25J27724L25424L24J25125024L25F25I27724Z25024H24Y25026827I27622125K21C21R1D25K27622025C26825C1N2772252602841N26027621S27728D27622425K28B1N28725C22025S25S28C28X28U28T28S27625H27B25V27724O25025024W24Z26Y26N26N24W24V24Y24U24O25124I26M24J24V24T27827721C27329R25C24Q21K23K25F25L27729O24Y24V27O24P24U27Q25G27725324Y24H24W25P2772822841428T22525S28H28Z21T28G21R28I27621X2AN2AS28Z22626G28H26G25C21S22L2682841R28B25C22K26G24025C1H2B225C22B21K26725C1P21K21R25B2892761327722K25S28B1628Z22K25K28X1U28T2261021R21R1E2C52BP28T1V28T');local o=bit and bit.bxor or function(l,e)local n,o=1,0
while l>0 and e>0 do
local c,a=l%2,e%2
if c~=a then o=o+n end
l,e,n=(l-c)/2,(e-a)/2,n*2
end
if l<e then l=e end
while l>0 do
local e=l%2
if e>0 then o=o+n end
l,n=(l-e)/2,n*2
end
return o
end
local function l(n,l,e)if e then
local l=(n/2^(l-1))%2^((e-1)-(l-1)+1);return l-l%1;else
local l=2^(l-1);return(n%(l+l)>=l)and 1 or 0;end;end;local e=1;local function n()local c,a,n,l=r(a,e,e+3);c=o(c,192)a=o(a,192)n=o(n,192)l=o(l,192)e=e+4;return(l*16777216)+(n*65536)+(a*256)+c;end;local function t()local l=o(r(a,e,e),192);e=e+1;return l;end;local function f()local e=n();local n=n();local c=1;local o=(l(n,1,20)*(2^32))+e;local e=l(n,21,31);local l=((-1)^l(n,32));if(e==0)then
if(o==0)then
return l*0;else
e=1;c=0;end;elseif(e==2047)then
return(o==0)and(l*(1/0))or(l*(0/0));end;return s(l,e-1023)*(c+(o/(2^52)));end;local h=n;local function s(l)local n;if(not l)then
l=h();if(l==0)then
return'';end;end;n=c(a,e,e+l-1);e=e+l;local e={}for l=1,#n do
e[l]=d(o(r(c(n,l,l)),192))end
return K(e);end;local e=n;local function r(...)return{...},u('#',...)end
local function K()local d={0,0,0,0,0,0,0,0,0,0,0,0,0};local e={0};local c={};local a={d,nil,e,nil,c};a[4]=t();for l=1,n()do e[l-1]=K();end;local e=n()local c={0,0,0,0,0};for n=1,e do
local e=t();local l;if(e==2)then l=(t()~=0);elseif(e==0)then l=f();elseif(e==3)then l=s();end;c[n]=l;end;a[2]=c
for a=1,n()do
local c=o(n(),136);local n=o(n(),215);local o=l(c,1,2);local e=l(n,1,11);local e={e,l(c,3,11),nil,nil,n};if(o==0)then e[3]=l(c,12,20);e[5]=l(c,21,29);elseif(o==1)then e[3]=l(n,12,33);elseif(o==2)then e[3]=l(n,12,32)-1048575;elseif(o==3)then e[3]=l(n,12,32)-1048575;e[5]=l(c,21,29);end;d[a]=e;end;return a;end;local function s(l,f,d)local e=l[1];local n=l[2];local o=l[3];local l=l[4];return function(...)local a=e;local c=n;local K=o;local o=l;local l=r
local n=1;local t=-1;local b={};local h={...};local u=u('#',...)-1;local r={};local e={};for l=0,u do
if(l>=o)then
b[l-o]=h[l+1];else
e[l]=h[l+1];end;end;local l=u-o+1
local l;local o;while true do
l=a[n];o=l[1];if o<=12 then if o<=5 then if o<=2 then if o<=0 then local o=l[2];local a=e[o+2];local c=e[o]+a;e[o]=c;if a>0 then if c<=e[o+1]then n=n+l[3];e[o+3]=c;end;elseif c>=e[o+1]then n=n+l[3];e[o+3]=c;end;elseif o>1 then local o=l[3];local n=e[o]for l=o+1,l[5]do n=n..e[l];end;e[l[2]]=n;else local n=l[2];local c={};local o=0;local l=n+l[3]-1;for l=n+1,l do o=o+1;c[o]=e[l];end;e[n](i(c,1,l-n));t=n;end;elseif o<=3 then local n=l[2];local c={};local o=0;local l=n+l[3]-1;for l=n+1,l do o=o+1;c[o]=e[l];end;e[n](i(c,1,l-n));t=n;elseif o>4 then local i=K[l[3]];local t;local o={};t=C({},{__index=function(e,l)local l=o[l];return l[1][l[2]];end,__newindex=function(n,l,e)local l=o[l]l[1][l[2]]=e;end;});for c=1,l[5]do n=n+1;local l=a[n];if l[1]==20 then o[c-1]={e,l[3]};else o[c-1]={f,l[3]};end;r[#r+1]=o;end;e[l[2]]=s(i,t,d);else e[l[2]]=f[l[3]];end;elseif o<=8 then if o<=6 then e[l[2]]=e[l[3]][c[l[5]]];elseif o==7 then local o=l[2];local c={};local n=0;local a=o+l[3]-1;for l=o+1,a do n=n+1;c[n]=e[l];end;local c={e[o](i(c,1,a-o))};local l=o+l[5]-2;n=0;for l=o,l do n=n+1;e[l]=c[n];end;t=l;else do return end;end;elseif o<=10 then if o>9 then e[l[2]]=d[c[l[3]]];else e[l[2]]();t=A;end;elseif o==11 then e[l[2]]=e[l[3]][c[l[5]]];else e[l[2]]=d[c[l[3]]];end;elseif o<=19 then if o<=15 then if o<=13 then do return end;elseif o==14 then local t=K[l[3]];local c;local o={};c=C({},{__index=function(e,l)local l=o[l];return l[1][l[2]];end,__newindex=function(n,l,e)local l=o[l]l[1][l[2]]=e;end;});for c=1,l[5]do n=n+1;local l=a[n];if l[1]==20 then o[c-1]={e,l[3]};else o[c-1]={f,l[3]};end;r[#r+1]=o;end;e[l[2]]=s(t,c,d);else e[l[2]]=c[l[3]];end;elseif o<=17 then if o==16 then e[l[2]]=c[l[3]];else local o=l[2];local a={};local n=0;local c=o+l[3]-1;for l=o+1,c do n=n+1;a[n]=e[l];end;local c={e[o](i(a,1,c-o))};local l=o+l[5]-2;n=0;for l=o,l do n=n+1;e[l]=c[n];end;t=l;end;elseif o==18 then local o=l[3];local n=e[o]for l=o+1,l[5]do n=n..e[l];end;e[l[2]]=n;else local o;e[l[2]]=c[l[3]];n=n+1;l=a[n];e[l[2]]=c[l[3]];n=n+1;l=a[n];e[l[2]]=c[l[3]];n=n+1;l=a[n];e[l[2]]=c[l[3]];n=n+1;l=a[n];o=l[2];e[o]=e[o]-e[o+2];n=n+l[3];end;elseif o<=22 then if o<=20 then e[l[2]]=e[l[3]];elseif o==21 then local o=l[2];e[o]=e[o]-e[o+2];n=n+l[3];else e[l[2]]=f[l[3]];end;elseif o<=24 then if o==23 then local o=l[2];e[o]=e[o]-e[o+2];n=n+l[3];else e[l[2]]();t=A;end;elseif o>25 then local u;local r;local s;local o;local h;local C;e[l[2]]=d[c[l[3]]];n=n+1;l=a[n];e[l[2]]=e[l[3]][c[l[5]]];n=n+1;l=a[n];e[l[2]]=c[l[3]];n=n+1;l=a[n];e[l[2]]=f[l[3]];n=n+1;l=a[n];C=l[3];h=e[C]for l=C+1,l[5]do h=h..e[l];end;e[l[2]]=h;n=n+1;l=a[n];o=l[2];s={};r=0;u=o+l[3]-1;for l=o+1,u do r=r+1;s[r]=e[l];end;e[o](i(s,1,u-o));t=o;n=n+1;l=a[n];do return end;else local o=l[2];local a=e[o+2];local c=e[o]+a;e[o]=c;if a>0 then if c<=e[o+1]then n=n+l[3];e[o+3]=c;end;elseif c>=e[o+1]then n=n+l[3];e[o+3]=c;end;end;n=n+1;end;end;end;return s(K(),{},b())();