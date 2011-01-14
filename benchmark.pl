% Simple benchmark utility.
%
% Example: benchmark_total(10**7, member(3, [1,2,3]), Time).

:- module(benchmark, [benchmark_average/3, benchmark_total/3]).

benchmark_average(N, Goal, AvgTime) :-
    benchmark_total(N, Goal, Time),
    AvgTime is Time / N.

benchmark_total(N, Goal, Time) :-
    statistics(cputime, Time0),
    run_n_times(N, Goal),
    statistics(cputime, Time1),
    Time is Time1 - Time0.

run_n_times(N, Goal) :-
    N1 is N, % So that we can pass 10**6.
    between(1, N1, _),
    Goal,
    fail.
run_n_times(_, _).
