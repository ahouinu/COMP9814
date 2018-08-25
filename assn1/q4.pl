% question 4
% Write a predicate chop_up(List, NewList) that takes List and binds NewList to
% List with all sequences of successive increasing whole numbers replaced by
% a two-item list containing only the first and last number in the sequence.

% list_successive_rest/3
% base case
list_successive_rest([X], [X], []).
% recursive case
list_successive_rest([A, B | X], [A], [B | X]) :-
    A + 1 =\= B.
list_successive_rest([A, B | X], [A | Successive], Rest) :-
    A + 1 =:= B,
    list_successive_rest([B | X], Successive, Rest).

% base case 1
% convert 1-element list to a 'naked' element
convert_list_1([A], NewList):-
    % length([A], 1),
    NewList = A.

% convert_list/2
% base case 2
convert_list([A, B], NewList):-
    % length([A, B], 2),
    NewList = [A, B].
% recursive case
convert_list([A, _ | Xs], NewList):-
    convert_list([A | Xs], NewList).

% chop_up/2
% base case
chop_up([], []).
% recursive case 1
chop_up(List, [Converted | Chopped]) :-
    list_successive_rest(List, Succ, Rest),
    length(Succ, 1),
    convert_list_1(Succ, Converted),
    chop_up(Rest, Chopped).
% recursive case 2
chop_up(List, [Converted | Chopped]) :-
    list_successive_rest(List, Succ, Rest),
    convert_list(Succ, Converted),
    chop_up(Rest, Chopped).
