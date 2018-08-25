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
