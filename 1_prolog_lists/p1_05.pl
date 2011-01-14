%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_05, [my_reverse/2]).

:- include('../common').

%%  my_reverse(+List, ?Reversed)
%
%   True if Reversed is List in reverse order.

test(my_reverse/2,
    [ true
    , my_reverse([], [])
    , my_reverse([a], [a])
    , my_reverse([a,b], [b,a])
    , my_reverse([a,b,c], [c,b,a])
    , my_reverse([a,b,c], R1), R1 == [c,b,a]
    , one:my_reverse([a,b,c], _)
    ]).

my_reverse(List, Reversed) :-
    my_reverse(List, [], Reversed).

my_reverse([], Rev, Rev).
my_reverse([X|Xs], Rev0, Rev) :-
    my_reverse(Xs, [X|Rev0], Rev).
