-module(server).
-export([start/1, stop/0]).

start(Port) ->
	% Start the server
	register(rudy, spawn(fun() -> rudy:init(Port) end)).

stop() ->
	% Stop the server.
	exit(whereis(rudy), "Closing server (brutally)").
