-module(brot).
-export([mandelbrot/2, mandelbrot_p/2, test_c/3, initiate/0]).


%% Complex number c and max iterations M
mandelbrot(C, M) ->
    Z0 = cmplx:new(0, 0),
    I = 0,
    test(I, Z0, C, M).

%% Performant method
mandelbrot_p({Cr, Ci}, M) ->
    %% Is the test even applicable?
    P = (Cr - 0.25),
    Q = P * P + Ci * Ci,
    Val = Q * (Q + P),
    Test = Ci * Ci * 0.25,
    if
	(Val > Test) -> 
	    test_p(0, 0, 0, 0, 0, Cr, Ci, M);
	true -> 
	    0
    end.


%% Test if this number is a member of the mandelbrot set.
%% Z0 = 0,
%% Z(n+1) = Z(n)^2 + C
%% 
%% If Z(n) >= 2 => infinite
%%
%% I - Iteration nr
%% Z - Z value
%% C - The complex number
%% M - Max number of iterations
test(I, _, _, M) when (I >= M)->
    0;
test(I, Z, C, M) ->
    %% we could remove the sqrt 
    Abs = cmplx:abs(Z),
    if
	(Abs >= 2) ->
	    I;
	true ->
	    test(I + 1, cmplx:add(cmplx:sqr(Z), C), C, M)
    end.


%% Hopefully more performant method.
test_p(I, _, _, _, _, _, _, M) when (I >= M)->
    0;
test_p(I, _, _, Zsr, Zsi, _, _, _) when ((Zsr + Zsi) >= 4)->
    I;
test_p(I, Zr, Zi, Zsr, Zsi, Cr, Ci, M) ->
    Tmp = (Zr * Zi),
    Znr = Zsr - Zsi + Cr,
    Zni = Tmp + Tmp + Ci,
    
    %% Stoping if we didn't update anything
    if
	((Zr == Znr) and (Zi == Zni)) ->
	    0;
	true -> 
	    test_p(I + 1, Znr, Zni, Znr * Znr, Zni * Zni, Cr, Ci, M)
    end.


%% Trying to build a cache
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initiate() ->
    Tree = gb_trees:empty(),
    spawn_link(fun () -> listener(Tree) end).

listener(Tree)->
    receive 
	{{Cr, Ci} = Key, M, Ret} ->
		io:write(Key),
		io:format("~n"),
	    case gb_trees:lookup(Key, Tree) of
		{value, Value} ->
		    Ret ! Value,
		    io:format("-"),
		    listener(Tree);
		none ->
		    Value = test_p(0, 0, 0, 0, 0, Cr, Ci, M),
		    Ret ! Value,
		    listener(gb_trees:insert(Key, Value, Tree))
		end
    end.
	    
test_c(C, M, Provider) ->
    Provider ! {C, M, self()},
    receive
	Var ->
	    Var
    end.
