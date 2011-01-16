%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_11, [encode_modified/2]).

:- use_module(p1_09, [pack/2]).

:- include('../common').

%%  encode_modified(+List, ?Encoding)
%
%   True if Encoding is a list of lists, where each list contains the
%   next consecutive duplicate sequence in List, reduced to only the
%   number of duplicates and a single instance of the value. Lists
%   that would contain only one element, e.g. [1,a], are replaced by
%   the element itself, e.g. a.

describe(encode_modified/2,
    [ true
    , encode_modified([], [])
    , encode_modified([a], [a])
    , encode_modified([a,a,a], [[3,a]])
    , encode_modified([a,a,a], R1), R1 == [[3,a]]
    , encode_modified(
        [a,a,a,a,b,c,c,a,a,d,e,e,e,e],
        [[4,a],b,[2,c],[2,a],d,[4,e]])
    , one:encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e], _)
    ]).

encode_modified(List, Encoding) :-
    pack(List, Groups),
    compact(Groups, Encoding).

compact([], []).
compact([[X]|Groups], [X|Codes]) :-
    !,
    compact(Groups, Codes).
compact([[X|Xs]|Groups], [[Len,X]|Codes]) :-
    length([X|Xs], Len), % SWI built-in.
    compact(Groups, Codes).
