%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_08, [compress/2, compress_v2/2]).

:- include('../common').

%   compress(+List, ?Compressed)
%
%   True if Compressed is a compressed version of List such that all
%   duplicate element sequences are compressed into only one element.

test(compress/2,
    [ true
    , compress([], [])
    , compress([a], [a])
    , compress([a,a], [a])
    , compress([a,a,a], [a])
    , compress([a,a,a,b], [a,b])
    , compress([a,a,a,b,b], [a,b])
    , compress([a,a,a,b,b,a], [a,b,a])
    , compress([a,a,a,b,b,a,a], [a,b,a])
    , compress([a,a,a,b,b,a,a], X), X = [a,b,a]
    , one:compress([a,a,a,b,b,a,a], _)
    ]).

compress([], []).
compress([X], [X]) :- !.
compress([X,X|T], T2) :-
    !,
    compress([X|T], T2).
compress([X,Y|T], [X|T2]) :-
    % X \= Y,
    compress([Y|T], T2).

%   compress_v2(+List, ?Compressed)
%
%   Alternative implementation of compress/2 without as much redundant
%   list consing (this was at least somewhat important long ago).
%
%   Same specification as compress/2.

test(compress_v2/2,
    [ true
    , compress_v2([], [])
    , compress_v2([a], [a])
    , compress_v2([a,a], [a])
    , compress_v2([a,a,a], [a])
    , compress_v2([a,a,a,b], [a,b])
    , compress_v2([a,a,a,b,b], [a,b])
    , compress_v2([a,a,a,b,b,a], [a,b,a])
    , compress_v2([a,a,a,b,b,a,a], [a,b,a])
    , compress_v2([a,a,a,b,b,a,a], X), X = [a,b,a]
    , one:compress_v2([a,a,a,b,b,a,a], _)
    ]).

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
