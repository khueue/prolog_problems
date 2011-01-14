%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_09, [pack/2]).

:- include('../common').

%%  pack(+List, ?Groups)
%
%   True if Groups is a list of lists, where the lists contain the
%   consecutive duplicates of List.

test(pack/2,
    [ true
    , pack([], [])
    , pack([a,a,a], [[a,a,a]])
    , pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],
           [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]])
    ]).
%/*
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
%*/

/*
pack([], []).
pack([X|Xs], Groups) :-
    pack(X, Xs, [], Groups).

pack(X, [], Group0, [[X|Group0]]).
pack(X, [X|Xs], Group0, Groups) :-
    !,
    pack(X, Xs, [X|Group0], Groups).
pack(X, [Y|Xs], Group0, [[X|Group0]|Groups]) :-
    % X \= Y,
    pack(Y, Xs, [], Groups).
*/
