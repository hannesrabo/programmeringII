-module(dinner).
-export([start/0, startBench/0]).

start() ->
    spawn(fun() -> init() end).

startBench() ->
    spawn(fun() -> initBench() end).

wait(0, _) ->
    io:format("Done!");
wait(N, Chopsticks) ->
    receive
        {done, Fails} ->
            io:fwrite("Fails: ~w~n", [Fails]),
            wait(N-1, Chopsticks);
        abort ->
            exit(abort)
    end.


initBench() ->
    C1 = chopstick:start(),
    C2 = chopstick:start(),
    C3 = chopstick:start(),
    C4 = chopstick:start(),
    C5 = chopstick:start(),
    Ctrl = self(),
    Waiter= waiter:start(),


    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 0, 1),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 0, 2),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 0, 3),

    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 1, 1),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 2, 1),

    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 1, 2),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 1, 3),

    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 2, 2),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 2, 3),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 2, 4),

    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 3, 3),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 4, 4),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 5, 5),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 5, 6),
    bench(C1, C2, C3, C4, C5, Waiter, Ctrl, 5, 7),

    io:format("~n Done ~n ------------------------------- ~n").

bench(C1, C2, C3, C4, C5, Waiter, Ctrl, TimeMin, TimeRand) ->
    Rounds = 100,
    io:format("Bench <~w> Min: ~w Rand: ~w~n", [Rounds, TimeMin, TimeRand]),
    T1 = erlang:system_time(milli_seconds),
    philosopher:startBench(Rounds, C1, C2, TimeMin, TimeRand, Waiter, Ctrl),
    philosopher:startBench(Rounds, C2, C3, TimeMin, TimeRand, Waiter, Ctrl),
    philosopher:startBench(Rounds, C3, C4, TimeMin, TimeRand, Waiter, Ctrl),
    philosopher:startBench(Rounds, C4, C5, TimeMin, TimeRand, Waiter, Ctrl),
    philosopher:startBench(Rounds, C5, C1, TimeMin, TimeRand, Waiter, Ctrl),
    wait(5, [C1, C2, C3, C4, C5]),
    T2 = erlang:system_time(milli_seconds),
    io:format("Time:"),
    io:write( (T2 - T1) ),
    io:format("~n").

init() ->
    C1 = chopstick:start(),
    C2 = chopstick:start(),
    C3 = chopstick:start(),
    C4 = chopstick:start(),
    C5 = chopstick:start(),
    Ctrl = waiter:start(),
    Rounds = 10,
    philosopher:start(Rounds, C1, C2, "Arendt", Ctrl),
    philosopher:start(Rounds, C2, C3, "Hypatia", Ctrl),
    philosopher:start(Rounds, C3, C4, "Simone", Ctrl),
    philosopher:start(Rounds, C4, C5, "Elizabeth", Ctrl),
    philosopher:start(Rounds, C5, C1, "Ayn", Ctrl),
    wait(5, [C1, C2, C3, C4, C5]).
