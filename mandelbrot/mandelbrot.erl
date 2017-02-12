-module(mandelbrot).
-export([demo/0, small_p/5, small_c/5]).

demo() ->
    io:format("Original: ~n"),
    small(-2.6, 1.2, 1.6),
    io:format("New and improved: ~n"),
    small_p(-2.6, 1.2, 1.6, 64, 7).

small(X, Y, X1) ->
    Width = 960,
    Height = 540,
    K = (X1 - X) / Width,
    Depth = 64,
    T0 = now(),
    Image = mandel:mandelbrot(Width, Height, X, Y, K, Depth),
    T = timer:now_diff(now(), T0),
    io:format("picture generated in ~w ms~n", [T div 1000]),
    ppm:write("./small.ppm", Image).

small_p(X, Y, X1, Depth, Threads) ->
    Width = 960,
    Height = 540,
    K = (X1 - X) / Width,
    T0 = now(),
    Image = mandel:mandelbrot_p(Width, Height, X, Y, K, Depth, Threads),
    T = timer:now_diff(now(), T0),
    io:format("picture_p generated in ~w ms~n", [T div 1000]),
    ppm:write("./small_p.ppm", Image).

small_c(X, Y, X1, Depth, Threads) ->
    Width = 960,
    Height = 540,
    K = (X1 - X) / Width,
    T0 = now(),
    Image = mandel:mandelbrot_c(Width, Height, X, Y, K, Depth, Threads),
    T = timer:now_diff(now(), T0),
    io:format("picture_c generated in ~w ms~n", [T div 1000]),
    ppm:write("./small_c.ppm", Image).

