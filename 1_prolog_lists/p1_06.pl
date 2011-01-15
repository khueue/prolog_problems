%   Prolog Problems - Prolog Lists
%   http://sites.google.com/site/prologsite/prolog-problems/1

:- module(p1_06, [is_palindrome/1]).

:- use_module(p1_05, [my_reverse/2]).

:- include('../common').

%%  is_palindrome(+List)
%
%   True if List is a palindrome.

describe(is_palindrome/1,
    [ true
    , is_palindrome([])
    , is_palindrome([a])
    , fail:is_palindrome([a,b])
    , is_palindrome([a,b,a])
    , is_palindrome([a,b,b,a])
    , is_palindrome([a,b,c,b,a])
    ]).

is_palindrome(List) :-
    my_reverse(List, List).
