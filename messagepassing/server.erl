-module(server).
-compile(export_all).

transmitter(Node, PName, Name) ->
    io:write(Node),
    io:fwrite("~n"),
    io:write(PName),
    io:fwrite("~n"),
    io:write(Name),
    io:fwrite("~n"),

    String = io:get_line(">"),
    {PName, Node} ! {message, String},
    io:fwrite("~n"),
    transmitter(Name, PName, Node).

receiver(Name) ->
    receive
        % {message, Var} ->
        %     io:format(Var);
        %
        {connect, Node, PName, Name} ->
            transmitter(Node, PName, Name);
        %
        Var ->
            io:write(Var)
    end,

    receiver(Name).

start() ->
    Name = io:get_line("Set Name: "),
    register(server, spawn(server, receiver, [Name])),
    io:fwrite("Waiting for a connection...").
