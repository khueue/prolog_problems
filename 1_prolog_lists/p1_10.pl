%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_10, [encode/2]).

:- use_module(p1_09, [pack/2]).

:- include('../common').

%%  encode(+List, ?Encoding)
%
%   True if Encoding is a list of lists, where each list contains the
%   next consecutive duplicate sequence in List, reduced to only the
%   number of duplicates and a single instance of the value. 

describe(encode/2,
    [ true
    , encode([], [])
    , encode([a], [[1,a]])
    , encode([a,a,a], [[3,a]])
    , encode([a,a,a], R1), R1 == [[3,a]]
    , encode(
        [a,a,a,a,b,c,c,a,a,d,e,e,e,e],
        [[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]])
    , one:encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e], _)
    ]).

encode(List, Encoding) :-
    pack(List, Groups),
    compact(Groups, Encoding).

compact([], []).
compact([[X|Xs]|Groups], [[Len,X]|Codes]) :-
    length([X|Xs], Len), % SWI built-in.
    compact(Groups, Codes).
