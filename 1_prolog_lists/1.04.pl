%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_04, [my_length/2]).

:- include('../common').

%%  my_length(+List, ?Length)
%
%   True if Length is the number of elements in List.

test(my_length/2,
    [ true
    , my_length([], 0)
    , my_length([_], 1)
    , my_length([_,_], 2)
    , my_length([_,_,_], 3)
    , my_length([_,_,_], L), L == 3
    , one:my_length([_,_,_], _)
    ]).

my_length(List, Length) :-
    my_length(List, 0, Length).

% Tail-recursive helper.
my_length([], Len, Len).
my_length([_|T], Len0, Len) :-
    Len1 is Len0 + 1,
    my_length(T, Len1, Len).
