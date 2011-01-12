%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_03, [element_at/3]).

:- include('../common').

%%  element_at(?X, +List, +Index)
%
%   True if X is the element at Index (1-based) in List.

test(element_at/3,
    [ true
    , fail:element_at(_, [a,b,c], -1)
    , fail:element_at(_, [a,b,c], 0)
    , fail:element_at(_, [a,b,c], 4)
    , element_at(a, [a,b,c], 1)
    , element_at(b, [a,b,c], 2)
    , element_at(c, [a,b,c], 3)
    , element_at(X, [a,b,c], 3), X == c
    , one:element_at(_, [a,b,c], 2)
    ]).

element_at(X, [X|_], 1) :- !.
element_at(X, [_|Xs], N) :-
    N > 1,
    N1 is N - 1,
    element_at(X, Xs, N1).
