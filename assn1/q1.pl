% question 1
% Write a predicate sumsq_neg(Numbers, Sum)
% that sums the squares of only the negative numbers in a list of numbers.
%

calc(Number, Sum, Rest) :-
    Number >= 0,
    Sum is Rest.

calc(Number, Sum, Rest) :-
    Number < 0,
    Sum is Rest + Number * Number.

% base case
%
sum_list([], 0).

% recursive case
%

sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    calc(H, Sum, Rest).

sumsq_neg(Numbers, Sum) :-
    sum_list(Numbers, Sum).
