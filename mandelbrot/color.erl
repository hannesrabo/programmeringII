-module(color).
-export([convert/2, fancy_conv/3]).

%% Gives us a color depending on the Depth and Max value
convert(Depth, Max) ->
    Frac = (Depth / Max) * 4, %% The faction of the section
    Sec = trunc(Frac), %% The section of the color range
    Offset = trunc(255 * (Frac - Sec)),
    
    case Sec of
	0 ->
	    {Offset, 0, 0};
	1 ->
	    {255, Offset, 0};
	2 ->
	    {255 - Offset, 255, 0};
	3 ->
	    {0, 255, Offset};
	4 ->
	    {0, 255 - Offset, 255}
    end.

fancy_conv(Depth, Max, Zn) ->
%%N + 1 - log (log  |Z(N)|) / log 2
    %% Inner = abs(math:log(cmplx:abs(Zn))) / math:log(2),
    %% Curve = (Depth + 1 - math:log(Inner)),
    Z_abs = abs(cmplx:abs(Zn)),
    if
	(Z_abs == 0) -> 
	    Log = 0;
	true -> 
	    %Log = abs( math:log(Z_abs)) / math:log(2)
	    Log = abs( math:log( abs( math:log(Z_abs)))) / math:log(2)
    end,
    
    if
	Depth == 0 -> Curve = 0, Value = 0;
	true -> Curve = (Depth + 1 - Log) / Max,
		Value = Curve + 1.5 * math:log(Depth) / math:log(Max)
    end,

    if
	Value > 1 -> V = 1;
	true -> V = Value
    end,
    


    hsv_to_rgb(Curve, 1, V).
    
hsv_to_rgb(Hue, Saturation, Value) ->
    C = Saturation * Value,
    X = C * ( 1 - abs((trunc(Hue * 6) rem 2) - 1)),
    M = Value - C,
    if
	Hue < 1/6 ->
	    {R,G,B} = {C, X, 0}; 
	Hue < 2/6 ->
	    {R,G,B} = {X, C, 0};
	Hue < 3/6 ->
	    {R,G,B} = {0, C, X};
	Hue < 4/6 ->
	    {R,G,B} = {0, X, C};
	Hue < 5/6 ->
	    {R,G,B} = {X, 0, C};	    
	true ->
	    {R,G,B} = {C, 0, X}
	end,

    {trunc((R + M) * 255), trunc((G + M) * 255), trunc((B + M) * 255)}.
