%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(problem_1_01, [my_last/2]).

my_last(X, [X]) :- !.
my_last(X, [_|Xs]) :-
    my_last(X, Xs).
