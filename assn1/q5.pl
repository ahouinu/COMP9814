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

% base cases
tree_eval(Value, tree(empty, z, empty), Value):-!.
tree_eval(_, tree(empty, Num, empty), Num).
% recursive case
tree_eval(Value, tree(Left, Op, Right), Result):-
    tree_eval(Value, Left, L),
    tree_eval(Value, Right, R),
    Expression =.. [Op, L, R],
    Result is Expression.
