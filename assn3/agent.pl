% agent.pl
% 18s1 COMP9414/9814 Assignment 3
% Option 2: BDI Agent
% Group Number: 258
% Team: Tianpeng Chen z5176343
%       Minjie Huang z5129528

% ------------------------------ Question 1 ------------------------------
% initial_intentions(Intentions)/1
%   bind Intentions to intents(L, []) with L in the form [[goal(X1,Y1),[]], ... , [goal(Xn,Yn),[]]]].
%   Here (Xn,Yn) is the location of the monster and
%   (X1,Y1), ... , (Xn-1,Yn-1) are places where the mininum number of stones need to be dropped
%   in order to allow the agent to move along some path from its current location to that of the monster.

% Declare s as a dynamic predicates, cuz it will be modified at runtime.
:- dynamic s/3.

% :- dynamic dropped/2.
% test :-
%     agent_at(X,Y),
%     write('  My Agent at: ('), write((X,Y)), writeln(')'),
%     world(World),
%     write('  My World: '), writeln(World).

initial_intentions(intents(L, [])) :-
    % retractall(s(_, _, 0)),         % disable special s/3
    agent_at(X0, Y0),               % get agent location (X0, Y0)
    assert(goal(goal(X0, Y0))),
    % assert(goal(goal(1, 1))),
    monster(Xn, Yn),                % get monster location (Xn, Yn)
    % solve(goal(9, 9), Path, _, _),
    solve(goal(Xn, Yn), Path, _, _),
    findall(X, (member(goal(X1, Y1), Path), not(land(X1, Y1)), X = [goal(X1, Y1), []]), L),
                                    % find all waters from Path,
                                    % insert into L in the form [goal, plan], [...]
    % retract(goal(goal(1, 1))),
    retract(goal(goal(X0, Y0))),
    % writeln(L),
    retractall(s(_, _, 1000)).      % retract s for waters,
                                    % we only concern lands later on.
    % test.

% Support Predicates
% s(goal, Nextgoal, Cost)/3
% for picking up stone at the same location
% s(goal(X, Y), goal(X, Y), 0) :-
%     agent_at(X, Y).
%
% cost is 1 for moving to a place on land
s(goal(X0, Y0), goal(X1, Y1), 1) :-
    % land(X1, Y1),
    land_or_dropped(X1, Y1),        % is land or dropped
    mandist(X0/Y0, X1/Y1, 1).       % Manhattan dist is 1
% cost is 1000 for moving to a place on water
s(goal(X0, Y0), goal(X1, Y1), 1000) :-
    Xs is X0 - 1,
    Xt is X0 + 1,
    Ys is Y0 - 1,
    Yt is Y0 + 1,
    between(Ys, Yt, Y1),            % find next Y
    between(Xs, Xt, X1),            % find next X
    % not(land(X1, Y1)),
    not(land_or_dropped(X1, Y1)),   % is water
    mandist(X0/Y0, X1/Y1, 1).       % Manhattan dist is 1
% set goal (location of agent 1,1)
% goal(goal(1,1)).

mandist(X/Y, X1/Y1, D) :-           % D is Manhattan Dist between two goalitions
    mdif(X, X1, Dx),
    mdif(Y, Y1, Dy),
    D is Dx + Dy.

mdif(A, B, D) :-                    % D is |A-B|
    D is A-B, D >= 0, !.

mdif(A, B, D) :-                    % D is |A-B|
    D is B-A.

% ------------------------------ Question 2 ------------------------------
% trigger(Percepts, Goals)/2
%   which takes a list of percepts, each of the form stone(X,Y),
%   and converts it into a corresponding list of goals, each of the form goal(X,Y).

% base case 1
%   empty lists
trigger([], []).
% base case 2
%   last element to copy
trigger([stone(X, Y)], [goal(X, Y)]) :-!.
% recursive case
%   takes the first stone(X, Y) of list Percepts and adds it to the head of list Goals
trigger([stone(X, Y) | Percepts_tail], [goal(X, Y) | Goals_tail]) :-
    trigger(Percepts_tail, Goals_tail).

% ------------------------------ Question 3 ------------------------------
% incorporate_goals(Goals, Intentions, Intentions1)/3
%   This procedure should take two inputs, as follows:
%   1. a set of Goals in the form of a list [goal(X1,Y1), ... , goal(Xn,Yn)]
%   2. the current Intentions of the agent, in the form intents(Int_drop,Int_pick)
%   where Int_drop, Int_pick are lists of intentions in the form [goal(X,Y), Plan]

% base case
% no more Goals, stop.
incorporate_goals([], Intentions, Intentions) :- !.

% recursive case 1
% If Goal is already in list Intentions, skip.
incorporate_goals([Goal | Tail], Intentions, Intentions1) :-
    % write('rec_1'),
    % write('Goal: '),
    % writeln(Goal),
    % writeln(Intentions),
    is_member(Goal, Intentions),
    incorporate_goals(Tail, Intentions, Intentions1).

% recursive case 2
% Else, insert Goal to list Intentions.
incorporate_goals([Goal | Tail], Intentions, Intentions1) :-
    % write('rec_2'),
    % write('Goal: '),
    % writeln(Goal),
    % write('Goals: '),
    % writeln([Goal | Tail]),
    not(is_member(Goal, Intentions)),
    % writeln(Goal),
    % writeln(Intentions),
    agent_at(X0, Y0),
    assert(goal(goal(X0, Y0))),
    retractall(s(_, _, 1000)),          % for passing Test 3...
    % write('Asserted: '), write((X0, Y0)), writeln(''),
    insert_intent(Goal, Intentions, UpdatedIntentions),
    % writeln(Goal),
    % writeln(Intentions),
    % writeln(UpdatedIntentions),
    retract(goal(goal(X0, Y0))),
    % write('Retracted: '), write((X0, Y0)), writeln(''),
    incorporate_goals(Tail, UpdatedIntentions, Intentions1).

% ----- Support Predicates -----
% is_member(Goal, Intentions)/2
%   Check whether Goal is in list Intentions.

% base case
%   found!
is_member(Goal, intents(_, [Head | _])) :-
    % write('Goal: '), writeln(Goal),
    % write('Head: '), writeln(Head),
    member(Goal, Head).
    % write('FOUND!').

% base case
%   stop
% is_member(_, intents([], _)) :- !.

% recursive case
is_member(Goal, intents(Int_drop, [Head | Tail])) :-
    % write('Goal: '), writeln(Goal),
    % write('Head: '), writeln(Head),
    not(member(Goal, Head)),
    % writeln(Goal),
    % writeln(Head),
    % writeln(Tail),
    is_member(Goal, intents(Int_drop, Tail)).

% insert_intent(Goal, Intentions, Intentions1)/3
insert_intent(Goal, Intentions, Intentions) :-
    not(solve(Goal, _, _, _)), !.

insert_intent(Goal, intents(Int_drop, Int_pick), intents(Int_drop, Int_pick_1)) :-
    % writeln('inserting...'),
    % writeln(Goal),
    solve(Goal, _, G, _),
    % write('Int_pick'), writeln(Int_pick),
    insert_intent_rec(Goal, G, Int_pick, Int_pick_1).

% insert_intent_rec(Goal, G, Int_pick, Int_pick_1)/3
% base case 1
%   No valid path found,
%   stop.
insert_intent_rec(Goal, _, [], [[Goal, []]]).
% insert_intent_rec(Goal, _, [[] | Tail], [[Goal, []] | Tail]).

% base case 2
  % if Int_pick == Int_pick_1,
  % stop.
% insert_intent_rec(Goal, _, [[Goal, Plan] | Tail], [[Goal, Plan] | Tail]).

% recursive case 1
%   if path length is not longer than G
%   keep trying...
insert_intent_rec(Goal, G, [[Pick_goal, Pick_plan] | Pick_tail], [[Pick_goal, Pick_plan] | Pick_tail_1]) :-
    % writeln('Hey1.1!'),
    solve(Pick_goal, _Path, G1, _),
    % writeln('Hey1.2!'),
    G >= G1,
    % write('New Path Not Found: '), writeln(Path),
    % write('G :'), write(G), write(' G1 :'), write(G1),
    % writeln(''),
    insert_intent_rec(Goal, G, Pick_tail, Pick_tail_1).

% recursive case 2
%   elif path length is longer than G,
%   insert new goal([Goal], []).
insert_intent_rec(Goal, G, [[Pick_goal, Pick_plan] | Pick_tail], [[Goal, []], [Pick_goal, Pick_plan] | Pick_tail]) :-
    % writeln('Hey2.1!'),
    solve(Pick_goal, _Path, G1, _),
    % write('Hey2.2!'),
    % write('New Path Found: '), writeln(Path),
    % write('G :'), write(G), write(' G1 :'), write(G1),
    % writeln(''),
    G < G1, !.


% ------------------------------ Question 4 ------------------------------
% get_action(Intentions, Intentions1, Action).% :-
    % which takes the agent's current Intentions in the form
    % intents(Int_drop, Int_pick) (as described above) and computes an action
    % to be taken by the agent as well as the updated Intentions. The agent
    % an intention as follows:
    % 1)
    % If the agent is currently holding a stone, indicated by agent_stones(1),
    % then the first intention [goal(X,Y), Plan] in the list Int_drop of
    % dropping intentions is selected;
    % 2)
    % otherwise, if the list Int_pick of picking intentions is not empty,
    % then its first item [goal(X,Y), Plan] is selected;
    % 3)
    % otherwise, no intention is selected; in this case,
    % the agent's Intentions should remain as they are, and it should stay in
    % its current location (i.e. action is move(X,Y) if it is currently at (X,Y)).

% Case 0: stay
get_action(intents(Int_drop, []), intents(Int_drop, []), move(X0, Y0)) :-
    agent_at(X0, Y0).
% get_action(intents([], Int_pick), intents([], Int_pick), move(X0, Y0)) :-
%     agent_at(X0, Y0).
% get_action(intents(Int_drop, [[goal(X0, Y0), []] | Pick_tail]), intents(Int_drop, [goal(X0, Y0) | Pick_tail]), move(X1, Y1)) :-
%     agent_at(X0, Y0),
%     X1 is X0 + 1,
%     Y1 is Y0,
%     applicable(move(X1, Y1)).
%
% get_action(intents(Int_drop, [[goal(X0, Y0), []] | Pick_tail]), intents(Int_drop, [goal(X0, Y0) | Pick_tail]), move(X1, Y1)) :-
%     agent_at(X0, Y0),
%     X1 is X0,
%     Y1 is Y0 + 1,
%     applicable(move(X1, Y1)).
%
% get_action(intents(Int_drop, [[goal(X0, Y0), []] | Pick_tail]), intents(Int_drop, [goal(X0, Y0) | Pick_tail]), move(X1, Y1)) :-
%     agent_at(X0, Y0),
%     X1 is X0 - 1,
%     Y1 is Y0,
%     applicable(move(X1, Y1)).
%
% get_action(intents(Int_drop, [[goal(X0, Y0), []] | Pick_tail]), intents(Int_drop, [goal(X0, Y0) | Pick_tail]), move(X1, Y1)) :-
%     agent_at(X0, Y0),
%     X1 is X0,
%     Y1 is Y0 - 1,
%     applicable(move(X1, Y1)).

% Case 1: agent is holding a stone, get the first intention in list Int_drop.
get_action(intents(Int_drop, Int_pick), intents(Int_drop_1, Int_pick), Action) :-
    agent_stones(1),
    agent_at(X0, Y0),
    assert(goal(goal(X0, Y0))),
    get_drop(Int_drop, Int_drop_1, Action),
    retract(goal(goal(X0, Y0))).
    % get_drop(Int_drop, Int_drop_1, Action).

% Case 2: agent has no stone, get the first intention in list Int_pick.
get_action(intents(Int_drop, Int_pick), intents(Int_drop, Int_pick_1), Action) :-
    agent_stones(0),
    agent_at(X0, Y0),
    assert(goal(goal(X0, Y0))),
    % writeln('Picking up'),
    % write('Int_pick: '), writeln(Int_pick),
    % write('Int_pick_1: '), writeln(Int_pick),
    % write('Action: '), writeln(Action),
    % assert(s(_, _, 0)),             % enable special s/3 to deal with the situation
                                    % when agent is trying to pick up a stone at
                                    % the same location.
    get_pick(Int_pick, Int_pick_1, Action),
    % retractall(s(_, _, 0)),         % disable special s/3
    retract(goal(goal(X0, Y0))).

% ----- Support Predicates -----
% get_drop(Int_drop, Int_drop_1, Action)/3
    % get the first intention in list Int_drop
% Case 0:
% get_drop([], [], move(X0, Y0)) :-
%     agent_at(X0, Y0).
% Case 1: Head_plan is applicable, Action = Head_plan, Int_drop_1 = rest of plans.
%         i.e. get first applicable plan as action.
get_drop([[goal(X, Y), [Head_plan | Tail_plan]] | Drop], [[goal(X, Y), Tail_plan] | Drop], Head_plan) :-
    applicable(Head_plan).

% Case 2: Head_plan is not applicable, find a new plan, append to New_plan at head.
%         i.e. generate a new plan list, get first one as Action
get_drop([[goal(X, Y), [Head_plan | _]] | Drop], [[goal(X, Y), New_plan] | Drop], Action) :-
    not(applicable(Head_plan)),
    % agent_at(X0, Y0),
    % assert(goal(goal(X0, Y0))),
    solve(goal(X, Y), Path, _, _),
    append([_|Tail_path], [_], Path),
    % retract(goal(goal(X0, Y0))),
    findall(Xs, (Xs = move(X1, Y1), member(goal(X1, Y1), Tail_path)), Tail_plan),
    append(Tail_plan, [drop(X, Y)], [Action | New_plan]).

% Case 3: No intention available, i.e. list Plan is empty
%         Almost the same as above, generate a new Plan list,
%         and get the first element as Action
get_drop([[goal(X, Y), []] | Drop], [[goal(X, Y), New_plan] | Drop], Action) :-
    % not(applicable(Head_plan)),
    % agent_at(X0, Y0),
    % assert(goal(goal(X0, Y0))),
    solve(goal(X, Y), Path, _, _),
    append([_|Tail_path], [_], Path),
    % retract(goal(goal(X0, Y0))),
    findall(Xs, (Xs = move(X1, Y1), member(goal(X1, Y1), Tail_path)), Tail_plan),
    append(Tail_plan, [drop(X, Y)], [Action | New_plan]).

% get_pick(Int_pick, Int_pick_1, Action)/3
    % get the first intention in list Int_pick
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% % Intentions = intents([[goal(5,3),[]],[goal(7,6),[]],[goal(8,6),[]],[goal(9,9),[]]],[[goal(5,2),[]]]),
% % |    get_action(Intentions, Intentions1, Action),
% % |    writeln(Intentions1),
% % |    writeln(Action).
% % intents([[goal(5,3),[]],[goal(7,6),[]],[goal(8,6),[]],[goal(9,9),[]]],[[goal(5,2),[pick(5,2)]]])
% % move(4,2)
% % Intentions = intents([[goal(5, 3), []], [goal(7, 6), []], [goal(8, 6), []], [goal(9, 9), []]], [[goal(5, 2), []]]),
% % Intentions1 = intents([[goal(5, 3), []], [goal(7, 6), []], [goal(8, 6), []], [goal(9, 9), []]], [[goal(5, 2), [pick(5, 2)]]]),
% % Action = move(4, 2) ;
% % false.

% Seems it can pass my single test when stone is under agent's feet,
% however, it still dead-loops sometimes when testing the game...
% Gave up here.

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% Case 0: stay (try to deal with the situation when stone appears the same location
%         as the agent).

get_pick([[goal(X, Y) ,[]]| Tail], [[goal(X, Y) ,[pick(X, Y)]]| Tail], move(X1, Y1)) :-
    agent_at(X, Y),
    % writeln('pick here:1 '),
    % X = X0,
    % Y = Y0,
    X1 is X - 1,
    Y1 is Y,
    applicable(move(X1, Y1)), !.
    % write('1 '), write(applicable(move(X1, Y1))), write('move '), write((X1, Y1)).
get_pick([[goal(X, Y) ,[]]| Tail], [[goal(X, Y) ,[pick(X, Y)]]| Tail], move(X1, Y1)) :-
    agent_at(X, Y),
    % writeln('pick here:2 '),
    % X = X0,
    % Y = Y0,
    X1 is X,
    Y1 is Y - 1,
    applicable(move(X1, Y1)), !.
    % write('2 '), write(applicable(move(X1, Y1))), write('move '), write((X1, Y1)).
get_pick([[goal(X, Y) ,[]]| Tail], [[goal(X, Y) ,[pick(X, Y)]]| Tail], move(X1, Y1)) :-
    agent_at(X, Y),
    % writeln('pick here:3 '),
    % X = X0,
    % Y = Y0,
    X1 is X + 1,
    Y1 is Y,
    applicable(move(X1, Y1)), !.
    % write('3 '), write(applicable(move(X1, Y1))), write('move '), write((X1, Y1)).
get_pick([[goal(X, Y) ,[]]| Tail], [[goal(X, Y) ,[pick(X, Y)]]| Tail], move(X1, Y1)) :-
    agent_at(X, Y),
    % writeln('pick here:4 '),
    % X = X0,
    % Y = Y0,
    X1 is X,
    Y1 is Y + 1,
    applicable(move(X1, Y1)), !.
    % write('4 '), write(applicable(move(X1, Y1))), write('move '), write((X1, Y1)).

% Case 1: Head_plan is applicable, Action = Head_plan, Int_drop_1 = rest of plans.
get_pick([[goal(X, Y), [Head_plan | Tail_plan]] | Pick], [[goal(X, Y), Tail_plan] | Pick], Head_plan) :-
    applicable(Head_plan).

% Case 2: Head_plan is not applicable, find a new plan, append to New_plan at head.
%         i.e. generate a new plan list, get first one as Action
get_pick([[goal(X, Y), [Head_plan | _]] | Pick], [[goal(X, Y), New_plan] | Pick], Action) :-
    not(applicable(Head_plan)),
    % agent_at(X0, Y0),
    % assert(goal(goal(X0, Y0))),
    solve(goal(X, Y), Path, _, _),
    append([_|Tail_path], [_], Path),
    % retract(goal(goal(X0, Y0))),
    findall(Xs, (Xs = move(X1, Y1), member(goal(X1, Y1), Tail_path)), Tail_plan),
    append(Tail_plan, [pick(X, Y)], [Action | New_plan]).

% Case 3: No intention available, i.e. list Plan is empty
%         Almost the same as above, generate a new Plan list,
%         and get the first element as Action
get_pick([[goal(X, Y), []] | Pick], [[goal(X, Y), New_plan] | Pick], Action) :-
    % not(applicable(Head_plan)),
    % agent_at(X0, Y0),
    % assert(goal(goal(X0, Y0))),
    solve(goal(X, Y), Path, _, _),
    % write('Path: '), writeln(Path),
    append([_|Tail_path], [_], Path),
    % retract(goal(goal(X0, Y0))),
    findall(Xs, (Xs = move(X1, Y1), member(goal(X1, Y1), Tail_path)), Tail_plan),
    append(Tail_plan, [pick(X, Y)], [Action | New_plan]),
    % write('goal: '), writeln(goal(X, Y)),
    % write('New Plan: '), writeln(New_plan),
    % write('New Action: '), writeln(Action).

% ------------------------------ Question 5 ------------------------------
% update_intentions(Observation, Intentions, Intentions1)/3
%   to update the agent's intentions, based on observation.
%   An at(X,Y) observation should not change the agent's intentions.
%   In the case of a picked() or dropped() observation,
%   the agent should remove the corresponding plan from its list of intentions
%   (since this plan has now successfully been executed).

% at(X, Y), do nothing
update_intentions(at(_, _), Intentions, Intentions).
% remove picked(X, Y).
update_intentions(picked(_, _), intents(Int_drop, [_ | Tail_pick]), intents(Int_drop, Tail_pick)).
% remove dropped(X, Y).
update_intentions(dropped(_, _), intents([_ | Tail_drop], Int_pick), intents(Tail_drop, Int_pick)).
% prevent backtracking
update_intentions(_, Intentions, Intentions).

% ---------------------------- ucsdijkstra.pl ----------------------------
% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.

solve(Start, Solution, G, N)  :-
    % consult(pathsearch), % insert_legs(), head_member(), build_path()
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N).

% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    goal(Node),
    build_path([[Node,Pred]|Expanded], Path).

% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).

% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).

% ----------------------------- pathsearch.pl -----------------------------
% pathsearch.pl

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% This file provides code for insert_legs(), head_member() and build_path()
% used by bfsdijkstra(), ucsdijkstra(), greedy() and astar().

% insert_legs(Generated, Legs, Generated1).
% insert new legs into list of generated legs,
% by repeatedly calling insert_one_leg()

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).
