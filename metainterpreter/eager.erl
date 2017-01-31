-module(eager).
-export([eval_expr/2, eval_match/3]).


% Evaluating a atom results in a atom
eval_expr({atm, Id}, _) ->
    {ok, Id};

% Evaluating an variable results in its value
eval_expr({var, Id}, Env) ->
    case env:lookup(Id, Env) of
        false ->
            error;
        {_, Val} ->
            {ok, Val}
    end;

% Evaluating the tuple
eval_expr({cons, Expr1, Expr2}, Env) ->
    case eval_expr(Expr1, Env) of
        error ->
            error;
        {ok, Val1} ->
            case eval_expr(Expr2, Env) of
                error ->
                    error;
                {ok, Val2} ->
                    {ok, [Val1|Val2]}
            end
    end.



% Pattern, Datastructure, Environment
eval_match(ignore, _, Env) ->
    {ok, Env};
eval_match({atm, Id}, {atm, Id}, Env) ->
    {ok, Env};
eval_match({var, Id}, Str, Env) ->
    case env:lookup(Id, Env) of
        false ->
            {ok, env:add(Id, Str, Env)};
        {Id, Str} ->
            {ok, Env};
        {Id, _} ->
            fail
    end;

eval_match({cons, Pat1, Pat2}, [H|T], Env) ->
    case eval_match(Pat1, H, Env) of
        fail ->
            fail;
        {ok, Env2} ->
            eval_match(Pat2, T, Env2)
    end;
eval_match(_, _, _) ->
    fail.
