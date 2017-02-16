-module(mandelbrot).
-export([demo/0, small_p/5, gen/6, gen_simple/3]).

demo() ->
    io:format("Original: ~n"),
    big(-2.6, 1.2, 1.6),
    io:format("New and improved: ~n"),
    gen(-2.6, 1.2, 1.6, 1024, 1920, 1080).

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

big(X, Y, X1) ->
    Width = 1920,
    Height = 1080,
    K = (X1 - X) / Width,
    Depth = 1024,
    T0 = now(),
    Image = mandel:mandelbrot(Width, Height, X, Y, K, Depth),
    T = timer:now_diff(now(), T0),
    io:format("picture generated in ~w ms~n", [T div 1000]),
    ppm:write("./small.ppm", Image).


gen(X, Y, X1, Depth, Width, Height) ->
    K = (X1 - X) / Width,
    T0 = now(),
    Image = mandel:mandelbrot_p(Width, Height, X, Y, K, Depth, 8),
    T = timer:now_diff(now(), T0),
    io:format("picture_p generated in ~w ms~n", [T div 1000]),
    ppm:write("./pic.ppm", Image).


gen_simple(Depth, Width, Height) ->
    K = (1.6 + 2.6) / Width,
    T0 = now(),
    Image = mandel:mandelbrot_p(Width, Height, -2.6, 1.2,K , Depth, 1),
    T = timer:now_diff(now(), T0),
    io:format("pic_simple generated in ~w ms~n", [T div 1000]),
    ppm:write("./pic_simple.ppm", Image).


small_p(X, Y, X1, Depth, Threads) ->
    Width = 960,
    Height = 540,
    K = (X1 - X) / Width,
    T0 = now(),
    Image = mandel:mandelbrot_p(Width, Height, X, Y, K, Depth, Threads),
    T = timer:now_diff(now(), T0),
    io:format("picture_p generated in ~w ms~n", [T div 1000]),
    ppm:write("./small_p.ppm", Image).

