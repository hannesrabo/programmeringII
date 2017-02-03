-module(client).
-compile(export_all).

transmit(Name, Node) ->
    io:write(Name),
    String = io:get_line(">"),
    {server, Node} ! {message, String},
    io:fwrite("~n"),
    transmit(Name, Node).

receiver(Name) ->
    receive
        {message, Var} ->
            io:format(Var)
    end,

    receiver(Name).

start() ->
    Node = io:get_line("Server IP: "),
    Name = io:get_line("Username: "),

    % Connecting
    {server, Node} ! {pid, self()},

    register(client, spawn(client, receiver, [Node])),
    transmit(Name, Node).
