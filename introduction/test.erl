-module(test).
-export([test/0,
         double/1,
         c/1,
         area/2,
         area/1,
         carea/1,
         product/2,
         product2/2,
         exp/2,
         fexp/2,
         nth/2,
         number/1,
         sum/1,
         duplicate/1,
         isort/1,
         bench/0,
         binaryList/1,
         fib/1,
         fibb/0
        ]).

test() ->
  io:fwrite("Hello world~n ").

double(N) ->
  N*2.

c(F) ->
  (F-32)/1.8.

area(B) ->
  area(B,B)*2.

area(B,H) ->
  B*H/2.

carea(R) ->
  math:pi()*math:pow(R, 2).

% if
% M == 0 -> ...;
% true -> ...
% end.
product(A, B) ->
  if
    A == 0 ->
      0;
    true ->
      product(A-1, B) + B
  end.

product2(0, _) ->
  0;
product2(A, B) ->
  product2(A-1, B) + B.

exp(0, _) ->
  0;
exp(_, 0) ->
  1;
exp(Base, Exp) ->
  Base*exp(Base, Exp - 1).

fexp(_, 0) ->
  1;

fexp(Base, Exp) when (Exp rem 2) == 0 ->
  fexp(Base,Exp div 2) * fexp(Base,Exp div 2);

fexp(Base, Exp) ->
  Base * fexp(Base, Exp - 1).

%nth(N, L): return the N’t element of the list L
% if N == 0 -> return H
% else return nth(n-1, T)

nth(0, L) ->
  [H|_] = L,
  H;

nth(N, L) ->
  [_|T] = L,
  nth(N-1, T).

% number(L): return the number of element in the list L
%if L == [] -> 0
%else
%[H|T]= L
%1 + number(T)

number(L) when L == [] ->
  0;
number(L) ->
  [_|T] = L,
  1 + number(T).

%sum(L): return the sum of all elements in the list L, assume that all elements are integers
sum([]) ->
  0;
sum([H|T]) ->
  H + sum(T).

%duplicate(L): return a list where all elements are duplicated

duplicate([]) ->
  [];
duplicate([H|T]) ->
  [H*2|duplicate(T)].


%Insertion - sorting a list
insert(Element, []) ->
  [Element];

insert(Element, [H|T] = List) ->
  if
    %If the element is smaller than the first
    Element =< H ->
      [Element|List];

    %Else
    true ->
      [H|insert(Element, T)]

  end.

isort(List) ->
  isort(List, []).

isort([], Sorted) ->
  Sorted;

isort(List, Sorted) ->
  [H|T] = List,
  isort(T, insert(H, Sorted)).


nreverse([]) ->
    [];
nreverse([H|T]) ->
    R = nreverse(T),
    [R|H].

reverse(L) ->
    reverse(L, []).

reverse([], R) ->
    R;

reverse([H|T], R) ->
    reverse(T, [H|R]).

bench() ->
  Ls = [16, 32, 64, 128, 256, 512, 1024, 2048, 4096],
  N = 100,
  Bench = fun(L) ->
    S = lists:seq(1,L),
    Tn = time(N, fun() -> nreverse(S) end),
    Tr = time(N, fun() -> reverse(S) end),
    io:format("length: ~10w nrev: ~8w us rev: ~8w us~n", [L, Tn, Tr])
  end,
  lists:foreach(Bench, Ls).

time(N, F)->
  %% time in micro seconds
  T1 = erlang:system_time(micro_seconds),
  loop(N, F),
  T2 = erlang:system_time(micro_seconds),
  (T2 -T1).

loop(N, Fun) ->
  if N == 0 ->
    ok;

  true ->
    Fun(),
    loop(N-1, Fun)

  end.

binaryList(Integer) ->
  binaryList(Integer, []).

binaryList(0, BinaryList) ->
  BinaryList;
binaryList(Integer, BinaryList) ->
  binaryList( Integer bsr 1, [ (Integer band 1) | BinaryList ] ).


fib(0) ->
  0;
fib(1) ->
  1;
fib(N) ->
  fib(N - 1 ) + fib ( N - 2).

fibb() ->
  Ls = [8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40],
  N = 10,

  Bench = fun(L) ->
    T = time(N, fun() -> fib(L) end),
    io:format("n: ~4w fib(n) calculated in: ~8w us~n", [L, T])
  end,

  lists:foreach(Bench, Ls).



  %
