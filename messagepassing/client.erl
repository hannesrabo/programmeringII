-module(client).
-compile(export_all).

transmit(ServerNode) ->
    String = "this is a message~n",
    {server, ServerNode} ! String.
