-module(philosopher).
-export([start/5, startBench/7]).

sleep(T, D) ->
    timer:sleep(T + rand:uniform(D)).

%% Create a new process
start(Hungry, Left, Right, Name, Waiter) ->
    spawn_link(fun() -> cycle(Hungry, Left, Right, Name, Waiter) end).

startBench(Hungry, Left, Right, MinTime, RandTime, Waiter, Ctrl) ->
    spawn_link(fun() -> benchCycle(Hungry, Left, Right, MinTime, RandTime, 0, Waiter, Ctrl) end).

benchCycle(0, _, _, _, _, FailedAttempts, _, Ctrl) ->
    Ctrl ! {done, FailedAttempts};
benchCycle(Hungry, Left, Right, MinTime, RandTime, FailedAttempts, Waiter, Ctrl) ->
    sleep(MinTime, RandTime),

    case waiter:request(Left, Right, Waiter) of
        ok ->
            case waiter:return(Left, Right, Waiter) of
                ok ->
                    benchCycle(Hungry - 1, Left, Right, MinTime, RandTime, FailedAttempts, Waiter, Ctrl);
                _ ->
                    benchCycle(Hungry, Left, Right, MinTime, RandTime, FailedAttempts + 1, Waiter, Ctrl)

            end;

        _ ->
            benchCycle(Hungry, Left, Right, MinTime, RandTime, FailedAttempts + 1, Waiter, Ctrl)
    end.

%% Process stuff to do
cycle(0, _, _, Name, _) ->
    io:format("==============> ~s is done~n", [Name]),
    done;

cycle(Hungry, Left, Right, Name, Waiter) ->
    sleep(1, 1),

    case waiter:request(Left, Right, Waiter) of
        ok ->
            io:format("~s Got the sticks~n", [Name]),

            case waiter:return(Left, Right, Waiter) of
                ok ->
                    io:format("~s returned the sticks~n", [Name]),
                    cycle(Hungry - 1, Left, Right, Name, Waiter);
                _ ->
                    io:format("~s failed at returning sticks~n", [Name]),
                    cycle(Hungry, Left, Right, Name, Waiter)

            end;

        _ ->
            io:format("~s  Failed to retriev ------------------------------~n", [Name]),
            cycle(Hungry, Left, Right, Name, Waiter)
    end.
