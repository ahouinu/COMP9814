% question 3
% Write a predicate sqrt_table(N, M, Result) that binds Result to
% the list of pairs consisting of a number and its square root,
% from N down to M, where N an d M are non-negative integers, and N >= M.

% base case
sqrt_table(M, M, Result):-
    X is sqrt(M),
    Result = [[M, X]].

% recursive case
sqrt_table(N, M, Result):-
    N >= M,
    X is sqrt(N),
    NewN is N-1,
    sqrt_table(NewN, M, NewResult),
    Result = [[N, X] | NewResult].
