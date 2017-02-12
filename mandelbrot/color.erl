-module(color).
-export([convert/2]).

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
