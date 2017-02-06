-module(eager).
-export([eval_expr/2, eval_match/3, eval/1]).


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
    end;

eval_expr({switch, Expr, List}, Env) ->
    case eval_switch(Expr, List, Env) of
        error ->
            error;

        {ok, Val} ->
            {ok, Val}
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


% [{switch, {atm, a}, [{{atm, b}, [{match, {var, x}, {atm, b}}]},{{atm, a}, [{match, {var, x}, {atm, c}}]}]}]
eval_switch(_, [], Env) ->
    {ok, Env};
eval_switch(Expr, [{Guard, ExecSeq}|ExpressionList], Env) ->
    io:fwrite("~n Expr:"),
    io:write(Expr),
    io:fwrite("~n Guard:"),
    io:write(Guard),

    case eval_match(Guard, Expr, Env) of
        fail ->
            io:fwrite("~n Error in match:"),
            eval_switch(Expr, ExpressionList, Env);
        {ok, Env2} ->
            io:fwrite("~n Evaluating:"),
            io:write(ExecSeq),
            case eval_seq(ExecSeq, Env2) of
                error ->
                    error;
                {_, Env3} ->
                    io:fwrite("~n"),
                    {ok, Env3}
            end
    end.

% Evaluation of sequences

eval(Seq) ->
    eval_seq(Seq, env:new() ).


eval_seq([{match, Ptr, Exp}|Seq], Env) ->
    case eval_expr(Exp, Env) of
        error ->
            error;

        {ok, Str} ->
            case eval_match(Ptr, Str, Env) of
                fail ->
                    error;
                {ok, Env2} ->
                    eval_seq(Seq, Env2)
            end
    end;
eval_seq([], Env) ->
    {ok, Env};

eval_seq([Exp], Env) ->
    {eval_expr(Exp, Env), Env}.
