-module(cmplx).
-export([new/2, add/2, sqr/1, abs/1]).

%% Create a new complex numer
new(X, Y) ->
    {X,Y}.

%% Add two numbers
add({A1, A2}, {B1, B2}) ->
    new(A1 + B1, A2 + B2).

%% Calculate the square of a complex number 
sqr({A1, A2}) ->
    new(A1*A1 - A2*A2, 2*A1*A2).

%% Calculate the absolute value of a complex number
abs({A1, A2}) ->
    math:sqrt(A1*A1 + A2*A2).
