% mycode.pl
% Written by Tianpeng Chen z5176343 for COMP9814 Assignment 1

% question 1
% Write a predicate sumsq_neg(Numbers, Sum)
% that sums the squares of only the negative numbers in a list of numbers.

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


% question 2
% Write a predicate all_like_all(Who_List, What_List)
% that takes a list of people Who_List and a list of items What_List
% and succeeds if every person in Who_List likes every item in What_List,
% according to the predicate likes(Who, What).
% Your predicate should also succeed if either Who_List or What_List is empty.


% i_like_all/2
% base case
i_like_all(_, []).
% recursive case
i_like_all(I, [Head | Tail]):-
    likes(I, Head),
    i_like_all(I, Tail).

% all_like_all/2
% base case
all_like_all([], []).
all_like_all(_, []).
all_like_all([], _).
% recursive case
all_like_all([WhoHead | WhoTail], What_List):-
    i_like_all(WhoHead, What_List),
    all_like_all(WhoTail, What_List).

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

% question 5
% For this question we consider binary expression-trees whose leaves are
% either of the form tree(empty, Num, empty) where Num is a number,
% or tree(empty, z, empty) in which case we will think of the letter z as
% a kind of "variable". Every tree is either a leaf or of the form tree(L, Op, R)
% where L and R are the left and right subtrees, and Op is one of the arithmetic
% operators '+', '-', '*', '/' (signifying addition, subtraction,
% multiplication and division).
% Write a predicate tree_eval(Value, Tree, Eval) that binds Eval to
% the result of evaluating the expression-tree Tree, with the variable z
% set equal to the specified Value.

% base cases (sorry for using cut :-( )
tree_eval(Value, tree(empty, z, empty), Value):-!.
tree_eval(_, tree(empty, Num, empty), Num).
% recursive case
tree_eval(Value, tree(Left, Op, Right), Result):-
    tree_eval(Value, Left, L),
    tree_eval(Value, Right, R),
    Expression =.. [Op, L, R],
    Result is Expression.
