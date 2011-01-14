%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_07, [my_flatten/2, my_flatten_v2/2]).

:- include('../common').

%%  my_flatten(+List, ?FlatList)
%
%   True if FlatList is List with all nesting removed.

test(my_flatten/2,
    [ true
    , my_flatten([], [])
    , my_flatten([a], [a])
    , my_flatten([a,[]], [a])
    , my_flatten([a,[b]], [a,b])
    , my_flatten([[1,2],a,[b]], [1,2,a,b])
    , my_flatten([[1,2],a,[b,[c,[d]]]], [1,2,a,b,c,d])
    , my_flatten([[1,2],a,[b,[c,[d]]]], F1), F1 == [1,2,a,b,c,d]
    , one:my_flatten([[1,2],a,[b,[c,[d]]]], _)
    ]).

my_flatten([], []).
my_flatten([X|Xs], Flat) :-
    looks_like_list(X),
    !,
    my_flatten(X, FlatX),
    my_flatten(Xs, FlatXs),
    append(FlatX, FlatXs, Flat). % SWI built-in.
my_flatten([X|Xs], [X|FlatXs]) :-
    % \+ looks_like_list(X),
    my_flatten(Xs, FlatXs).

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

%%  my_flatten_v2(+List, ?FlatList)
%
%   Alternative version of flatten that has no append.
%
%   Same specification as my_flatten/2.

test(my_flatten_v2/2,
    [ true
    , fail:my_flatten_v2([], [[]])
    , my_flatten_v2([], [])
    , my_flatten_v2([a], [a])
    , my_flatten_v2([a,[]], [a])
    , my_flatten_v2([a,[b]], [a,b])
    , my_flatten_v2([[1,2],a,[b]], [1,2,a,b])
    , my_flatten_v2([[1,2],a,[b,[c,[d]]]], [1,2,a,b,c,d])
    , my_flatten_v2([[1,2],a,[b,[c,[d]]]], F1), F1 == [1,2,a,b,c,d]
    , one:my_flatten_v2([[1,2],a,[b,[c,[d]]]], _)
    ]).

my_flatten_v2(List, Flat) :-
    my_flatten_v2(List, [], Flat).

my_flatten_v2([], Flat0, Flat) :-
    !,
    Flat0 = Flat. % Makes sure the first test goal succeeds.
my_flatten_v2([X|Xs], Flat0, Flat) :-
    !,
    my_flatten_v2(X, FlatXs, Flat),
    my_flatten_v2(Xs, Flat0, FlatXs).
my_flatten_v2(X, Flat0, [X|Flat0]).
    % \+ looks_like_list(X).
