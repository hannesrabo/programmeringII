-module(server).
-compile(export_all).

transmitter(Name, Node) ->
    io:write(Name),
    String = io:get_line(">"),
    {client, Node} ! {message, String},
    io:fwrite("~n"),
    transmitter(Name, Node).

receiver(Name) ->
    receive
        {message, Var} ->
            io:format(Var);

        {pid, Pid} ->
            transmitter(Name, Pid);

        Var ->
            io:write(Var)
    end,

    receiver(Name).

start() ->
    Name = io:get_line("Set Name: "),
    register(server, spawn(server, receiver, [Name])),
    io:fwrite("Waiting for a connection...").
