-module(derive).
-export([derivative/2]).

deriv({const, _}, _) ->
  {const, 0};

%If we are calculating derivative of this variable
deriv({var, V}, V) ->
  {const, 1};

%If it's not the current variable
deriv({var, _}, _) ->
  {const, 0};

%Derivative of add function
deriv({add, Ex1, Ex2}, V) ->
  {add, deriv(Ex1, V), deriv(Ex2, V)};

%Derivative of multiplication function
deriv({mul, Ex1, Ex2}, V) ->
  S = simplify({add, {mul, deriv(Ex1, V), Ex2}, {mul, Ex1, deriv(Ex2, V) } }),
  S;

deriv({pot, Base, Exponent}, V) ->
  {mul, {mul, Exponent, {pot, Base, {add, Exponent, {const, -1}} } }, deriv(Base, V) };

deriv({ln, Ex}, V) ->
  {mul, {pot, Ex, {const, -1}}, deriv(Ex, V) };

deriv({sqrt, Ex}, V) ->
  {mul, {pot, {mul, {const, 2}, {sqrt, Ex}}, -1}, deriv(Ex, V) };

deriv({sin, Ex}, V) ->
  {mul, {cos, Ex}, deriv(Ex, V)};

deriv({cos, Ex}, V) ->
  {mul, {mul, {sin, Ex}, deriv(Ex, V)}, {const, -1}}.


%Simplifying expressions
simplify({var, V}) ->
  {var, V};
simplify({const, V}) ->
  {const, V};

%Powers
simplify({pot, Ex, {const, 1}}) ->
  simplify(Ex);
simplify({pot, _, {const, 0}}) ->
  {const, 1};
simplify({pot, {const, Base}, {const, Exponent}}) ->
  {const, math:pow(Base, Exponent) };

%Multiplication
simplify({mul, {const, 0}, _}) ->
  {const, 0};
simplify({mul, _, {const, 0}}) ->
  {const, 0};
simplify({mul, {const, K1}, {const, K2}}) ->
  {const, K1*K2};
simplify({mul, {const, K1}, {mul, {const, K2}, Ex}}) ->
  {mul, {const, K1*K2}, Ex};
simplify({mul, {const, K1}, {mul, Ex, {const, K2}}}) ->
  {mul, {const, K1*K2}, Ex};
simplify({mul, {mul, {const, K2}, Ex}, {const, K1}}) ->
  {mul, {const, K1*K2}, Ex};
simplify({mul, {mul, Ex, {const, K2}}, {const, K1}}) ->
  {mul, {const, K1*K2}, Ex};

%Addition
simplify({add, {const, 0}, Ex}) ->
  simplify(Ex);
simplify({add, Ex, {const, 0}}) ->
  simplify(Ex);
simplify({add, {const, K1}, {const, K2}}) ->
  {const, K1+K2};
simplify({add, {mul, {const, K1}, Expr}, {mul, {const, K2}, Expr}}) ->
  {mul, {const, K1+K2}, Expr};

%General (for expressions)

%Stop-statements
simplify({Op, {var, V}, {var, U}}) ->
  {Op, {var, V}, {var, U}};
simplify({Op, {const, K}, {var, V}}) ->
  {Op, {const, K}, {var, V}};
simplify({Op, {var, V}, {const, K}}) ->
  {Op, {var, V}, {const, K}};

%If there are any expressions left
simplify({Op, Ex1, Ex2}) ->
  E1 = simplify(Ex1),
  E2 = simplify(Ex2),
  if
    (E1 /= Ex1) or (E2 /= Ex2)->
      simplify({Op, E1, E2});
    true ->
      {Op, E1, E2}
  end.


derivative(Ex, V) ->
  Res1 = simplify(deriv(Ex, V)),
  Res = simplify(Res1),
  io:fwrite("Expression:~n"),
  print(Ex),
  io:fwrite("~nData Out:~n"),
  io:write(Res),
  io:fwrite("~nDerivative:~n"),
  print(Res),
  io:fwrite("~n~n").

print({var, V}) ->
  io:write(V);
print({const, K}) ->
  io:write(K);
print({sin, E1}) ->
  io:fwrite("sin("),
  print(E1),
  io:fwrite(")");
print({cos, E1}) ->
  io:fwrite("cos("),
  print(E1),
  io:fwrite(")");
print({ln, E1}) ->
  io:fwrite("ln("),
  print(E1),
  io:fwrite(")");
print({add, E1, E2}) ->
  print(E1),
  io:fwrite("+"),
  print(E2);
print({mul, E1, E2}) ->
  print(E1),
  io:fwrite("*"),
  print(E2);
print({pot, E1, E2}) ->
  io:fwrite("("),
  print(E1),
  io:fwrite(")^("),
  print(E2),
  io:fwrite(")").
