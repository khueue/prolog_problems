%   Simple benchmarking utility.
%
%   Currently only works with SWI-Prolog.
%
%   Example: bench_total(10**7, member(3, [1,2,3]), Time).

:- module(benchmark, [bench_average/3, bench_total/3]).

%%  bench_average(+N, +Goal, -AvgTime)
%
%   True if AvgTime is the average time in seconds (float) it takes to
%   execute Goal. Executes it N times and divides the total time by N.
%   N will be evaluated by is/2, so that we can give it e.g. 10**6.

bench_average(N, Goal, AvgTime) :-
    bench_total(N, Goal, Time),
    AvgTime is Time / N.

%%  bench_total(+N, +Goal, -TotalTime)
%
%   True if TotalTime is the total time in seconds (float) it takes to
%   execute Goal N times.
%   N will be evaluated by is/2, so that we can give it e.g. 10**6.

bench_total(N, Goal, Time) :-
    statistics(cputime, Time0),
    run_n_times(N, Goal),
    statistics(cputime, Time1),
    Time is Time1 - Time0.

run_n_times(N, Goal) :-
    M is integer(N), % So that we can pass 10**6.
    between(1, M, _),
    call(Goal),
    fail.
run_n_times(_, _).
