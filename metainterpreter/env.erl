-module(env).
-export([new/0, add/3, lookup/2]).


% Create a new empty environment
% a environment is represented as:
%
new () ->
    {}.


% Return a new environment with the binding added (Id => Str)
add(Id, Str, {}) ->
    {{Id, Str}, {}, {}};
add(Id, Str, {Element = {Eid, _ }, El, Er}) ->
    if
        % If the id has a lower value -> to the left
        Id < Eid ->
            {Element, add(Id, Str, El), Er};
        % else - to the right
        true ->
            {Element, El, add(Id, Str, Er)}
    end.

% Check if this Id has a binding
% returns {Id, Val} if it has or false
lookup(_, {}) ->
    false;
lookup(Id, {{Id, Ev}, _, _}) ->
    {Id, Ev};
lookup(Id,{{Eid, _}, El, Er}) ->
    if
        Id < Eid ->
            lookup(Id, El);
        true ->
            lookup(Id, Er)
    end.
