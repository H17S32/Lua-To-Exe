
local Byte         = string.byte;
local Char         = string.char;
local Sub          = string.sub;
local Concat       = table.concat;
local LDExp        = math.ldexp;
local GetFEnv      = getfenv or function() return _ENV end;
local Setmetatable = setmetatable;
local Select       = select;

local Unpack = unpack;
local ToNumber = tonumber;local function decompress(b)local c,d,e="","",{}local f=256;local g={}for h=0,f-1 do g[h]=Char(h)end;local i=1;local function k()local l=ToNumber(Sub(b, i,i),36)i=i+1;local m=ToNumber(Sub(b, i,i+l-1),36)i=i+l;return m end;c=Char(k())e[1]=c;while i<#b do local n=k()if g[n]then d=g[n]else d=c..Sub(c, 1,1)end;g[f]=c..Sub(d, 1,1)e[#e+1],c,f=d,d,f+1 end;return table.concat(e)end;local ByteString=decompress('25C25D25C27627727625F27725F25E27724V24Z25F25J27724L25424L24J25125024L25F25I27724Z25024H24Y25026827I27622125K21C21R1D25K27622025C26825C1N2772252602841N26027621S27728D27622425K28B1N28725C22025S25S28C28X28U28T28S27625H27B25V27724O25025024W24Z26Y26N26N24W24V24Y24U24O25124I26M24J24V24T27827721C27329R25C24Q21K23K25F25L27729O24Y24V27O24P24U27Q25G27725324Y24H24W25P2772822841428T22525S28H28Z21T28G21R28I27621X2AN2AS28Z22626G28H26G25C21S22L2682841R28B25C22K26G24025C1H2B225C22B21K26725C1P21K21R25B2892761327722K25S28B1628Z22K25K28X1U28T2261021R21R1E2C52BP28T1V28T');

local BitXOR = bit and bit.bxor or function(a,b)
    local p,c=1,0
    while a>0 and b>0 do
        local ra,rb=a%2,b%2
        if ra~=rb then c=c+p end
        a,b,p=(a-ra)/2,(b-rb)/2,p*2
    end
    if a<b then a=b end
    while a>0 do
        local ra=a%2
        if ra>0 then c=c+p end
        a,p=(a-ra)/2,p*2
    end
    return c
end

local function gBit(Bit, Start, End)
	if End then
		local Res = (Bit / 2 ^ (Start - 1)) % 2 ^ ((End - 1) - (Start - 1) + 1);

		return Res - Res % 1;
	else
		local Plc = 2 ^ (Start - 1);

        return (Bit % (Plc + Plc) >= Plc) and 1 or 0;
	end;
end;

local Pos = 1;

local function gBits32()
    local W, X, Y, Z = Byte(ByteString, Pos, Pos + 3);

	W = BitXOR(W, 192)
	X = BitXOR(X, 192)
	Y = BitXOR(Y, 192)
	Z = BitXOR(Z, 192)

    Pos	= Pos + 4;
    return (Z*16777216) + (Y*65536) + (X*256) + W;
end;

local function gBits8()
    local F = BitXOR(Byte(ByteString, Pos, Pos), 192);
    Pos = Pos + 1;
    return F;
end;

local function gFloat()
	local Left = gBits32();
	local Right = gBits32();
	local IsNormal = 1;
	local Mantissa = (gBit(Right, 1, 20) * (2 ^ 32))
					+ Left;
	local Exponent = gBit(Right, 21, 31);
	local Sign = ((-1) ^ gBit(Right, 32));
	if (Exponent == 0) then
		if (Mantissa == 0) then
			return Sign * 0; -- +-0
		else
			Exponent = 1;
			IsNormal = 0;
		end;
	elseif (Exponent == 2047) then
        return (Mantissa == 0) and (Sign * (1 / 0)) or (Sign * (0 / 0));
	end;
	return LDExp(Sign, Exponent - 1023) * (IsNormal + (Mantissa / (2 ^ 52)));
end;

local gSizet = gBits32;
local function gString(Len)
    local Str;
    if (not Len) then
        Len = gSizet();
        if (Len == 0) then
            return '';
        end;
    end;

    Str	= Sub(ByteString, Pos, Pos + Len - 1);
    Pos = Pos + Len;

	local FStr = {}
	for Idx = 1, #Str do
		FStr[Idx] = Char(BitXOR(Byte(Sub(Str, Idx, Idx)), 192))
	end

    return Concat(FStr);
end;

local gInt = gBits32;
local function _R(...) return {...}, Select('#', ...) end

local function Deserialize()
    local Instrs = { 0,0,0,0,0,0,0,0,0,0,0,0,0 };
    local Functions = { 0 };
	local Lines = {};
    local Chunk = 
	{
		Instrs,
		nil,
		Functions,
		nil,
		Lines
	};Chunk[4] = gBits8();for Idx=1,gBits32() do Functions[Idx-1]=Deserialize();end;
								local ConstCount = gBits32()
    							local Consts = {0,0,0,0,0};

								for Idx=1,ConstCount do 
									local Type=gBits8();
									local Cons;
	
									if(Type==2) then Cons=(gBits8() ~= 0);
									elseif(Type==0) then Cons = gFloat();
									elseif(Type==3) then Cons=gString();
									end;
									
									Consts[Idx]=Cons;
								end;
								Chunk[2] = Consts
								for Idx=1,gBits32() do 
									local Data1=BitXOR(gBits32(),136);
									local Data2=BitXOR(gBits32(),215); 

									local Type=gBit(Data1,1,2);
									local Opco=gBit(Data2,1,11);
									
									local Inst=
									{
										Opco,
										gBit(Data1,3,11),
										nil,
										nil,
										Data2
									};

									if (Type == 0) then Inst[3]=gBit(Data1,12,20);Inst[5]=gBit(Data1,21,29);
									elseif(Type==1) then Inst[3]=gBit(Data2,12,33);
									elseif(Type==2) then Inst[3]=gBit(Data2,12,32)-1048575;
									elseif(Type==3) then Inst[3]=gBit(Data2,12,32)-1048575;Inst[5]=gBit(Data1,21,29);
									end;
									
									Instrs[Idx]=Inst;end;return Chunk;end;
local function Wrap(Chunk, Upvalues, Env)
	local Instr  = Chunk[1];
	local Const  = Chunk[2];
	local Proto  = Chunk[3];
	local Params = Chunk[4];

	return function(...)
		local Instr  = Instr; 
		local Const  = Const; 
		local Proto  = Proto; 
		local Params = Params;

		local _R = _R
		local InstrPoint = 1;
		local Top = -1;

		local Vararg = {};
		local Args	= {...};

		local PCount = Select('#', ...) - 1;

		local Lupvals	= {};
		local Stk		= {};

		for Idx = 0, PCount do
			if (Idx >= Params) then
				Vararg[Idx - Params] = Args[Idx + 1];
			else
				Stk[Idx] = Args[Idx + 1];
			end;
		end;

		local Varargsz = PCount - Params + 1

		local Inst;
		local Enum;	

		while true do
			Inst		= Instr[InstrPoint];
			Enum		= Inst[1];if Enum <= 12 then if Enum <= 5 then if Enum <= 2 then if Enum <= 0 then local A=Inst[2];local Step=Stk[A+2];local Index=Stk[A]+Step;Stk[A]=Index;if Step>0 then if Index<=Stk[A+1] then InstrPoint=InstrPoint+Inst[3];Stk[A+3]=Index;end;elseif Index>=Stk[A+1] then InstrPoint=InstrPoint+Inst[3];Stk[A+3]=Index;end; elseif Enum > 1 then local B=Inst[3];local K=Stk[B] for Idx=B+1,Inst[5] do K=K..Stk[Idx];end;Stk[Inst[2]]=K;else local A=Inst[2];local Args={};local Edx=0;local Limit=A+Inst[3]-1;for Idx=A+1,Limit do Edx=Edx+1;Args[Edx]=Stk[Idx];end;Stk[A](Unpack(Args,1,Limit-A));Top=A;end; elseif Enum <= 3 then local A=Inst[2];local Args={};local Edx=0;local Limit=A+Inst[3]-1;for Idx=A+1,Limit do Edx=Edx+1;Args[Edx]=Stk[Idx];end;Stk[A](Unpack(Args,1,Limit-A));Top=A; elseif Enum > 4 then local NewProto=Proto[Inst[3]];local NewUvals;local Indexes={};NewUvals=Setmetatable({},{__index=function(_,Key)local Val=Indexes[Key];return Val[1][Val[2]];end,__newindex=function(_,Key,Value)local Val=Indexes[Key] Val[1][Val[2]]=Value;end;});for Idx=1,Inst[5] do InstrPoint=InstrPoint+1;local Mvm=Instr[InstrPoint];if Mvm[1]==20 then Indexes[Idx-1]={Stk,Mvm[3]};else Indexes[Idx-1]={Upvalues,Mvm[3]};end;Lupvals[#Lupvals+1]=Indexes;end;Stk[Inst[2]]=Wrap(NewProto,NewUvals,Env);else Stk[Inst[2]]=Upvalues[Inst[3]];end; elseif Enum <= 8 then if Enum <= 6 then Stk[Inst[2]]=Stk[Inst[3]][Const[Inst[5]]]; elseif Enum == 7 then local A=Inst[2];local Args={};local Edx=0;local Limit=A+Inst[3]-1;for Idx=A+1,Limit do Edx=Edx+1;Args[Edx]=Stk[Idx];end;local Results={Stk[A](Unpack(Args,1,Limit-A))};local Limit=A+Inst[5]-2;Edx=0;for Idx=A,Limit do Edx=Edx+1;Stk[Idx]=Results[Edx];end;Top=Limit;else do return end;end; elseif Enum <= 10 then if Enum > 9 then Stk[Inst[2]]=Env[Const[Inst[3]]];else Stk[Inst[2]]();Top=A;end; elseif Enum == 11 then Stk[Inst[2]]=Stk[Inst[3]][Const[Inst[5]]];else Stk[Inst[2]]=Env[Const[Inst[3]]];end; elseif Enum <= 19 then if Enum <= 15 then if Enum <= 13 then do return end; elseif Enum == 14 then local NewProto=Proto[Inst[3]];local NewUvals;local Indexes={};NewUvals=Setmetatable({},{__index=function(_,Key)local Val=Indexes[Key];return Val[1][Val[2]];end,__newindex=function(_,Key,Value)local Val=Indexes[Key] Val[1][Val[2]]=Value;end;});for Idx=1,Inst[5] do InstrPoint=InstrPoint+1;local Mvm=Instr[InstrPoint];if Mvm[1]==20 then Indexes[Idx-1]={Stk,Mvm[3]};else Indexes[Idx-1]={Upvalues,Mvm[3]};end;Lupvals[#Lupvals+1]=Indexes;end;Stk[Inst[2]]=Wrap(NewProto,NewUvals,Env);else Stk[Inst[2]]=Const[Inst[3]];end; elseif Enum <= 17 then if Enum == 16 then Stk[Inst[2]]=Const[Inst[3]];else local A=Inst[2];local Args={};local Edx=0;local Limit=A+Inst[3]-1;for Idx=A+1,Limit do Edx=Edx+1;Args[Edx]=Stk[Idx];end;local Results={Stk[A](Unpack(Args,1,Limit-A))};local Limit=A+Inst[5]-2;Edx=0;for Idx=A,Limit do Edx=Edx+1;Stk[Idx]=Results[Edx];end;Top=Limit;end; elseif Enum == 18 then local B=Inst[3];local K=Stk[B] for Idx=B+1,Inst[5] do K=K..Stk[Idx];end;Stk[Inst[2]]=K;else local A;Stk[Inst[2]]=Const[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Const[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Const[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Const[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];Stk[A]=Stk[A]-Stk[A+2];InstrPoint=InstrPoint+Inst[3];end; elseif Enum <= 22 then if Enum <= 20 then Stk[Inst[2]]=Stk[Inst[3]]; elseif Enum == 21 then local A=Inst[2];Stk[A]=Stk[A]-Stk[A+2];InstrPoint=InstrPoint+Inst[3];else Stk[Inst[2]]=Upvalues[Inst[3]];end; elseif Enum <= 24 then if Enum == 23 then local A=Inst[2];Stk[A]=Stk[A]-Stk[A+2];InstrPoint=InstrPoint+Inst[3];else Stk[Inst[2]]();Top=A;end; elseif Enum > 25 then local Limit;local Edx;local Args;local A;local K;local B;Stk[Inst[2]]=Env[Const[Inst[3]]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Stk[Inst[3]][Const[Inst[5]]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Const[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];Stk[Inst[2]]=Upvalues[Inst[3]];InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];B=Inst[3];K=Stk[B] for Idx=B+1,Inst[5] do K=K..Stk[Idx];end;Stk[Inst[2]]=K;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];A=Inst[2];Args={};Edx=0;Limit=A+Inst[3]-1;for Idx=A+1,Limit do Edx=Edx+1;Args[Edx]=Stk[Idx];end;Stk[A](Unpack(Args,1,Limit-A));Top=A;InstrPoint = InstrPoint + 1;Inst = Instr[InstrPoint];do return end;else local A=Inst[2];local Step=Stk[A+2];local Index=Stk[A]+Step;Stk[A]=Index;if Step>0 then if Index<=Stk[A+1] then InstrPoint=InstrPoint+Inst[3];Stk[A+3]=Index;end;elseif Index>=Stk[A+1] then InstrPoint=InstrPoint+Inst[3];Stk[A+3]=Index;end;end;
			InstrPoint	= InstrPoint + 1;
		end;
    end;
end;	
return Wrap(Deserialize(), {}, GetFEnv())();
