-module(waiter).

start(Table) ->
    spawn_link( fun() -> waiter(Table) end ).


waiter(Table) ->
    receive
        {request, Left, Right} ->
            checkRequest(Table, From)
    end.


checkRequest()

request(Left, Right, Waiter) ->
    Waiter ! {request, self()}.
