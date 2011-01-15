%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_07, [flatten/2, flatten2/2, flatten3/2]).

:- include('../common').

%%  flatten(+Tree, ?List)
%
%   True if List is Tree with all nesting removed.
%
%   ?- bench_total(10**6, flatten([[1,2],a,[b,[c,[d]]]], _), Time).
%   Time = 3.82.

describe(flatten/2,
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
    my_append(FlatX, FlatXs, Flat).
flatten([X|Xs], [X|FlatXs]) :-
    % \+ looks_like_list(X),
    flatten(Xs, FlatXs).

%   my_append(+List1, +List2, ?List1List2)
%   my_append(?List1, ?List2, +List1List2)
%
%   True if List1List2 is the list concatenation of List1 and List2.

describe(my_append/3,
    [ true
    , my_append([], [], [])
    , my_append([a,b,c], [], [a,b,c])
    , my_append([], [a,b,c], [a,b,c])
    , my_append([a,b,c], [d,e], [a,b,c,d,e])
    , my_append([a,b,c], [d,e], L1), L1 == [a,b,c,d,e]
    , one:my_append([a,b,c], [d,e], _)
    ]).

my_append([], L, L).
my_append([X|T1], L2, [X|T3]) :-
    my_append(T1, L2, T3).

%%  flatten2(+Tree, ?List)
%
%   Alternative version of flatten that has no append.
%   Same specification as flatten/2.
%
%   ?- bench_total(10**6, flatten2([[1,2],a,[b,[c,[d]]]], _), Time).
%   Time = 2.74.

describe(flatten2/2,
    [ true
    , fail:flatten2([], [[]])
    , flatten2([], [])
    , flatten2([a], [a])
    , flatten2([a,[]], [a])
    , flatten2([a,[b]], [a,b])
    , flatten2([[1,2],a,[b]], [1,2,a,b])
    , flatten2([[1,2],a,[b,[c,[d]]]], [1,2,a,b,c,d])
    , flatten2([[1,2],a,[b,[c,[d]]]], F1), F1 == [1,2,a,b,c,d]
    , one:flatten2([[1,2],a,[b,[c,[d]]]], _)
    ]).

flatten2(Tree, Flat) :-
    flatten2(Tree, [], Flat).

flatten2([], Flat0, Flat) :-
    !,
    Flat0 = Flat. % Makes sure the first test goal passes.
flatten2([X|Xs], Flat0, Flat) :-
    !,
    flatten2(X, FlatXs, Flat),
    flatten2(Xs, Flat0, FlatXs).
flatten2(X, Flat0, [X|Flat0]).
    % \+ looks_like_list(X).

%%  flatten3(+Tree, ?List)
%
%   Less hacky, at the expense of a looks_like_list/1 call.
%   Same specification as flatten/2.
%
%   ?- bench_total(10**6, flatten3([[1,2],a,[b,[c,[d]]]], _), Time).
%   Time = 3.55.

describe(flatten3/2,
    [ true
    , fail:flatten3([], [[]])
    , flatten3([], [])
    , flatten3([a], [a])
    , flatten3([a,[]], [a])
    , flatten3([a,[b]], [a,b])
    , flatten3([[1,2],a,[b]], [1,2,a,b])
    , flatten3([[1,2],a,[b,[c,[d]]]], [1,2,a,b,c,d])
    , flatten3([[1,2],a,[b,[c,[d]]]], F1), F1 == [1,2,a,b,c,d]
    , one:flatten3([[1,2],a,[b,[c,[d]]]], _)
    ]).

flatten3(Tree, Flat) :-
    flatten3(Tree, [], Flat).

flatten3([], Flat, Flat) :- !.
flatten3([X|Xs], Flat0, Flat) :-
    !,
    flatten3(X, FlatXs, Flat),
    flatten3(Xs, Flat0, FlatXs).
flatten3(X, Flat0, [X|Flat0]) :-
    \+ looks_like_list(X).

%   looks_like_list(+List)
%
%   True if List "looks like" a list, meaning that it can be unified with
%   either [] or [_|_]. This is done in constant time, but does not ensure
%   that the list is well-formed (unlike the SWI built-in, is_list/1,
%   which recursively checks the entire list).

describe(looks_like_list/1,
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
