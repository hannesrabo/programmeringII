-module(server).
-compile(export_all).

receiver() ->
    receive
        Var ->
            io:format(Var),
            receiver()
    end.
start() ->
    register(server, spawn(server, receiver, [])).
