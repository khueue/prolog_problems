%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_02, [last_but_one/2]).

:- include('../common').

%   last_but_one(?LastButOneElement, +List)
%
%   True if LastButOneElement is the last but one element of List.

test(last_but_one/2,
    [ true
    , fail:last_but_one(_, [])
    , fail:last_but_one(_, [a])
    , last_but_one(a, [a,b])
    , last_but_one(b, [a,b,c])
    , last_but_one(X, [a,b,c]), X == b
    , one:last_but_one(_, [a,b,c])
    ]).

last_but_one(X, [X,_]) :- !.
last_but_one(X, [_|Xs]) :-
    last_but_one(X, Xs).
