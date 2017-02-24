-module(mandel).
-export([mandelbrot/6, mandelbrot_p/7]).

mandelbrot(Width, Height, X, Y, K, Depth) ->
    Trans = fun(W, H) ->
		    cmplx:new(X + K*(W-1), Y - K*(H-1))
	    end,
    lists:reverse(rows(0, Width, Height, Trans, Depth, [])).


%% Generate the rows (backwards)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Base: If we are at the end of the height
rows(Y, _, Y, _, _, List) ->
    List;
rows(Y, Width, Height, Trans, Depth, List) ->
    Row = lists:reverse(row(0, Y, Width, Height, Trans, Depth, [])),
    rows(Y + 1, Width, Height, Trans, Depth, [Row| List]).
    


%% Generate a row of pixels (backwards)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% If we reached the end of the row
row(X, _, X, _, _, _, List) ->
    List;

row(X, Y, Width, Height, Trans, Depth, List) ->
    %% Generate the complex number associated with the pixel + 
    %% Calculate the "depth"
    Num = brot:mandelbrot(Trans(X, Y), Depth),
    
    %% Convert to color
    Col = color:convert(Num, Depth),
    
    %% Insert into list
    row(X + 1, Y, Height, Width, Trans, Depth, [Col | List]).




%% Cutting the picture in multiple parts and calculate simultaniously
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mandelbrot_p(Width, Height, X, Y, K, Depth, Threads) ->
    T1 = X - K,
    T2 = Y + K,
    Trans = fun(W, H) ->
		    cmplx:new(T1 + K * W, T2 - K * H)
	    end,
    Chunk = trunc(Height / Threads + 1),
    %% Distribute problem over many threads
    io:format("Spawning ~w threads~n", [Threads]),
    mandel_spawn_threads(Width, 0, Chunk, Height, Trans, Depth, 
			 0, self()),

    %% Receive data from all threads
    %% Sort the data on the first element (id)
    List = lists:keysort(1, mandelbrot_receiver(Threads, [])),
    %% Create the list
    mandelbrot_create_list(List).
 
%% Create the list from received data
mandelbrot_create_list([]) ->
    [];
mandelbrot_create_list([{_, Data} | Tail]) ->
    Data ++ mandelbrot_create_list(Tail).


%% Receive data from threads
mandelbrot_receiver(0, List) ->
    io:format("~nDone! ~n"),
    List;
mandelbrot_receiver(Threads, List) ->
    receive
	Data ->
	    io:format("."),
	    mandelbrot_receiver(Threads - 1, [Data | List])
	end.

mandel_spawn_threads(_, Start, _, Height, _, _, _, _) 
  when (Start >= Height) ->
    ok;
mandel_spawn_threads(Width, Start, Chunk, Height, Trans, Depth, Id, Collector)->
    %% Calculate the height of this chunk
    
    Temp = Start + Chunk,
    if
	(Temp < Height) -> 
	    StopHeight = Temp;
	true ->
	    StopHeight = Height
    end,
    
    %% Spawn thread
    spawn(fun() -> rows_pr(Start, Width, StopHeight, Trans, 
			   Depth, Id, Collector) 
	  end),

    %%Continue to spawn threads
    mandel_spawn_threads(Width, Start + Chunk, Chunk, Height, Trans, 
			 Depth, Id+1, Collector).


%% Generate the image chunk (and reverse it)
rows_pr(Start, Width, StopHeight, Trans, Depth, Id, Collector) ->
    List = [],
    Data = lists:reverse(rows_p(Start, Width, StopHeight, Trans, Depth, List)),
    Collector ! {Id, Data}.

%% Generate the rows (backwards)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Base: If we are at the end of the height
rows_p(Y, _, Y, _, _, List) ->
    List;
rows_p(Y, Width, Height, Trans, Depth, List) ->
    Row = lists:reverse(row_p(0, Y, Width, Height, Trans, Depth, [])),
    rows_p(Y + 1, Width, Height, Trans, Depth, [Row| List]).
    


%% Generate a row of pixels (backwards)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% If we reached the end of the row
row_p(X, _, X, _, _, _, List) ->
    List;

row_p(X, Y, Width, Height, Trans, Depth, List) ->
    %% Generate the complex number associated with the pixel + 
    %% Calculate the "depth"
    
%   {Num_t, Z} = brot:mandelbrot_p(Trans(X, Y), Depth),
    {Cr, Ci} = Trans(X, Y),
    {Num, _Zr, _Zi} = brot:test_c(float(Cr), float(Ci), Depth),
    %% Convert to color
    %% Col = color:convert(Num, Depth),
%   io:format("~w ~w ~w ~w~n", [Num_t, Num, Cr, Ci]),

%   Col = color:fancy_conv(Num, Depth, {Zr, Zi}),
	Col = color:convert(Num, Depth),

    %% Insert into list
    row_p(X + 1, Y, Width, Height, Trans, Depth, [Col | List]).



