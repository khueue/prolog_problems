%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(problem_1_08, [compress/2, compress_v2/2]).

:- include('../common').

compress([], []).
compress([X], [X]) :- !.
compress([X,X|T], T2) :-
    !,
    compress([X|T], T2).
compress([X,Y|T], [X|T2]) :-
    % X \= Y,
    compress([Y|T], T2).

% Alternative, without duplicate consing:

compress_v2([], []).
compress_v2([X|T], L) :-
    compress_v2(X, T, L).

compress_v2(X, [], [X]) :- !.
compress_v2(X, [X|T], L2) :-
    !,
    compress_v2(X, T, L2).
compress_v2(X, [Y|T], [X|T2]) :-
    % X \= Y,
    compress_v2(Y, T, T2).
