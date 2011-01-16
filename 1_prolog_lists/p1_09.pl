%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_09, [pack/2, pack2/2, pack3/2]).

:- include('../common').

%%  pack(+List, ?Groups)
%
%   True if Group is a list of lists, where each list contains the
%   next consecutive duplicate sequence in List.
%
%   ?- bench_total(10**6, pack([a,a,a,b,b,c,d,d,d,d], _), Time).
%   Time = 2.82.

describe(pack/2,
    [ true
    , pack([], [])
    , pack([a,a,a], [[a,a,a]])
    , pack([a,a,a], P1), P1 == [[a,a,a]]
    , pack(
        [a,a,a,a,b,c,c,a,a,d,e,e,e,e],
        [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]])
    , one:pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e], _)
    ]).

pack(List, Groups) :-
    pack(List, [], Groups).

pack([], Group, Group).
pack([X], Group0, [[X|Group0]]) :- !.
pack([X,X|Xs], Group0, Groups) :-
    !,
    pack([X|Xs], [X|Group0], Groups).
pack([X,Y|Xs], Group0, [[X|Group0]|Groups]) :-
    % X \= Y,
    pack([Y|Xs], [], Groups).

%%  pack2(+List, ?Groups)
%
%   Extra argument in the helper to avoid unnecessary consing.
%   Same specification as pack/2.
%
%   ?- bench_total(10**6, pack2([a,a,a,b,b,c,d,d,d,d], _), Time).
%   Time = 2.11.

describe(pack2/2,
    [ true
    , pack2([], [])
    , pack2([a,a,a], [[a,a,a]])
    , pack2([a,a,a], P1), P1 == [[a,a,a]]
    , pack2(
        [a,a,a,a,b,c,c,a,a,d,e,e,e,e],
        [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]])
    , one:pack2([a,a,a,a,b,c,c,a,a,d,e,e,e,e], _)
    ]).

pack2([], []).
pack2([X|Xs], Groups) :-
    pack2(Xs, X, [], Groups).

pack2([], X, Group0, [[X|Group0]]).
pack2([X|Xs], X, Group0, Groups) :-
    !,
    pack2(Xs, X, [X|Group0], Groups).
pack2([Y|Xs], X, Group0, [[X|Group0]|Groups]) :-
    % X \= Y,
    pack2(Xs, Y, [], Groups).

%%  pack3(+List, ?Groups)
%
%   Naive version using body recursion. Just for fun.
%   Same specification as pack/2.
%
%   ?- bench_total(10**6, pack3([a,a,a,b,b,c,d,d,d,d], _), Time).
%   Time = 12.02.

describe(pack3/2,
    [ true
    , pack3([], [])
    , pack3([a,a,a], [[a,a,a]])
    , pack3([a,a,a], P1), P1 == [[a,a,a]]
    , pack3(
        [a,a,a,a,b,c,c,a,a,d,e,e,e,e],
        [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]])
    , one:pack2([a,a,a,a,b,c,c,a,a,d,e,e,e,e], _)
    ]).

pack3([], []).
pack3([X], [[X]]) :- !.
pack3([X|Xs], [[X|Group]|Packed]) :-
    pack3(Xs, [Group|Packed]),
    Group = [X|_],
    !.
pack3([X|Xs], [[X],Group|Packed]) :-
    pack3(Xs, [Group|Packed]).
    % X \= head of Group.
