-module(philosopher).
-export([start/5]).

sleep(T, D) ->
    timer:sleep(T + rand:uniform(D)).

%% Create a new process
start(Hungry, Left, Right, Name, Ctrl) ->
    spawn_link(fun() -> cycle(Hungry, Left, Right, Name, Ctrl) end).

%% Process stuff to do
cycle(0, _, _, Name, _) ->
    io:format("==============> ~s is done~n", [Name]),
    done;

cycle(Hungry, Left, Right, Name, Ctrl) ->
    sleep(1, 1),
    Timeout = 3,

    case chopstick:request(Left, Timeout) of
        %% If we successfully got the chopsticks
        ok ->
            io:format("~s got the left~n", [Name]),
            case chopstick:request(Right, Timeout) of
                %% If we successfully got the chopsticks
                ok ->
                    io:format("~s got the right~n", [Name]),
                    %% Next cycle
                    chopstick:return(Left),
                    chopstick:return(Right),
                    cycle(Hungry-1, Left, Right, Name, Ctrl);

                %% Release chopsticks if we couldn't get the right one
                no ->
                    io:format("~s returned left without use~n", [Name]),
                    chopstick:return(Left),
                    cycle(Hungry, Left, Right, Name, Ctrl)

            end;

        no ->
            io:format("~s trying again~n", [Name]),
            cycle(Hungry, Left, Right, Name, Ctrl)

    end.
