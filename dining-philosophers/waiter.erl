-module(waiter).
-export([start/0, request/3, return/3]).

start() ->
    spawn_link( fun() -> waiter([]) end ).


waiter(Table) ->
    receive
        {request, Left, Right, From} ->
            handleRequest(Table, Left, Right, From);

        {return, Left, Right, From} ->
            handleRemove(Table, Left, Right, From)
    end.

%% Check for presence in list
inList(_, []) ->
    false;
inList(Element, [Element|_]) ->
    true;
inList (Element, [_|T]) ->
    inList(Element, T).

%% Handle a single connection to the waiter.
handleRequest(Table, Left, Right, From) ->
    Timeout = 1,
    case inList(Left, Table) of
        false ->
            case inList(Right, Table) of

                %% Non of the chopsticks are gone: give them to the requester.
                false ->
                    Msg = {chopstick:request(Left, Timeout), chopstick:request(Right, Timeout)},
                    From ! Msg,
                    %% Return control
                    if
                        Msg == {ok, ok} ->
                            waiter([Left, Right|Table]);
                        true ->
                            waiter(Table)
                    end;

                true ->
                    From ! no,
                    waiter(Table)
            end;
        true ->
            From ! no,
            waiter(Table)
    end.

%% Remove the items from the request lists to free resources
removeItem(_, []) ->
    [];
removeItem(Item, [Item|T]) ->
    T;
removeItem(Item, [H|T]) ->
    [H|removeItem(Item, T)].

%% Handle the remove event
handleRemove(Table, Left, Right, From) ->
    Msg =  {chopstick:return(Left),
            chopstick:return(Right)},
    From ! Msg,
    if
        %% Return control
        Msg == {ok, ok} ->
            waiter(removeItem(Right, removeItem(Left, Table)));
        true ->
            io:format("Could not return sticks"),
            waiter(Table)
    end.


%% Encapsulates the asyncronous request
request(Left, Right, Waiter) ->
    Waiter ! {request, Left, Right, self()},
    receive
        {ok, ok} ->
            ok;
        %% Everything except all ok
        _ ->
            no
    end.

%% Encapsulate asyncronous return
return(Left, Right, Waiter) ->
    Waiter ! {return, Left, Right, self()},
    receive
        {ok, ok} ->
            ok;
        _ ->
            no
    end.
