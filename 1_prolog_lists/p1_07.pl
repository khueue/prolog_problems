%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_07, [flatten/2, flatten_v2/2]).

:- include('../common').

%%  flatten(+Tree, ?List)
%
%   True if List is Tree with all nesting removed.

test(flatten/2,
    [ true
    , flatten([], [])
    , flatten([a], [a])
    , flatten([a,[]], [a])
    , flatten([a,[b]], [a,b])
    , flatten([[1,2],a,[b]], [1,2,a,b])
    , flatten([[1,2],a,[b,[c,[d]]]], [1,2,a,b,c,d])
    , flatten([[1,2],a,[b,[c,[d]]]], F1), F1 == [1,2,a,b,c,d]
    , one:flatten([[1,2],a,[b,[c,[d]]]], _)
    ]).

flatten([], []).
flatten([X|Xs], Flat) :-
    looks_like_list(X),
    !,
    flatten(X, FlatX),
    flatten(Xs, FlatXs),
    append(FlatX, FlatXs, Flat). % SWI built-in.
flatten([X|Xs], [X|FlatXs]) :-
    % \+ looks_like_list(X),
    flatten(Xs, FlatXs).

%   looks_like_list(+List)
%
%   True if List "looks like" a list, meaning that it can be unified with
%   either [] or [_|_]. This is done in constant time, but does not ensure
%   that the list is well-formed (unlike the SWI built-in, is_list/1,
%   which recursively checks the entire list).

test(looks_like_list/1,
    [ true
    , fail:looks_like_list(_)
    , fail:looks_like_list(a)
    , looks_like_list([])
    , looks_like_list([a])
    , looks_like_list([a,b])
    , looks_like_list([a|b]) % Oh, the price we pay for constant time!
    ]).

looks_like_list(0) :- % Catch variables.
    !,
    fail.
looks_like_list([]).
looks_like_list([_|_]).

%%  flatten_v2(+Tree, ?List)
%
%   Alternative version of flatten that has no append.
%
%   Same specification as flatten/2.

test(flatten_v2/2,
    [ true
    , fail:flatten_v2([], [[]])
    , flatten_v2([], [])
    , flatten_v2([a], [a])
    , flatten_v2([a,[]], [a])
    , flatten_v2([a,[b]], [a,b])
    , flatten_v2([[1,2],a,[b]], [1,2,a,b])
    , flatten_v2([[1,2],a,[b,[c,[d]]]], [1,2,a,b,c,d])
    , flatten_v2([[1,2],a,[b,[c,[d]]]], F1), F1 == [1,2,a,b,c,d]
    , one:flatten_v2([[1,2],a,[b,[c,[d]]]], _)
    ]).

flatten_v2(List, Flat) :-
    flatten_v2(List, [], Flat).

flatten_v2([], Flat0, Flat) :-
    !,
    Flat0 = Flat. % Makes sure the first test goal succeeds.
flatten_v2([X|Xs], Flat0, Flat) :-
    !,
    flatten_v2(X, FlatXs, Flat),
    flatten_v2(Xs, Flat0, FlatXs).
flatten_v2(X, Flat0, [X|Flat0]).
    % \+ looks_like_list(X).
