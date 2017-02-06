-module(chopstick).
-export([request/2, return/1, quit/1, start/0]).

start() ->
    spawn_link(fun() -> available() end).



available() ->
    receive
        {request, From} ->
            From ! granted,
            gone(From);
        quit ->
            ok
    end.

gone(From) ->
    receive
        return ->
            From ! returned,
            available();
        quit ->
            ok
    end.


request(Stick, Timeout) ->
    Stick ! {request, self()},

    receive
        granted ->
            ok
    after
        Timeout ->
            no
    end.

return(Stick) ->
    Stick ! return,

    receive
        returned ->
            ok
    end.

quit(Stick) ->
    Stick ! quit.
