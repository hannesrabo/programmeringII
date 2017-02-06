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
    SNode = io:get_line("Server name@host: "),
    Node = list_to_atom(string:left(SNode, string:len(SNode) - 1)),
    Name = io:get_line("Username: "),

    PName = '123', %list_to_atom(io_lib:format(rand:uniform(500))),
    Pid = spawn(client, receiver, [Node]),
    register(PName, Pid),

    % Connecting
    io:fwrite("~nConnecting to host: "),
    io:write(Node),
    Message = {connect, node(), PName, Name},
    {server, Node} ! Message.

    % transmit(Name, Node).
