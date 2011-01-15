%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_01, [my_last/2]).

:- include('../common').

%%  my_last(?LastElement, +List)
%
%   True if LastElement is the last element of List.

describe(my_last/2,
    [ true
    , fail:my_last(_, [])
    , my_last(a, [a])
    , my_last(b, [a,b])
    , my_last(c, [a,b,c])
    , my_last(X, [a,b,c]), X == c
    , one:my_last(_, [a,b,c])
    ]).

my_last(X, [X]) :- !.
my_last(X, [_|Xs]) :-
    my_last(X, Xs).
