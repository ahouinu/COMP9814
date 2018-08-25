% test 1
[land], [agent],
initial_intentions(Intentions),
Percepts = [],
trigger(Percepts, Goals),
incorporate_goals(Goals, Intentions, Intentions1).

% test 2
[land], [agent].
initial_intentions(Intentions),
Percepts = [stone(4,3), stone(4,2)],
trigger(Percepts, Goals),
incorporate_goals(Goals, Intentions, Intentions1).

% test 3
[land], [agent].
initial_intentions(Intentions),
Percepts = [stone(7,6)],
trigger(Percepts, Goals),
incorporate_goals(Goals, Intentions, Intentions1).

% test 4
[land], [agent].
init_world,
initial_intentions(Intentions),
Percepts = [stone(5,3)],
trigger(Percepts, [H|T]),
write('Goals: '),
writeln(Goals),
write('Intentions: '),
writeln(Intentions),
is_member(H, Intentions).

% test 5
[land], [agent].
initial_intentions(Intentions),
Percepts = [stone(5,3)],
trigger(Percepts, Goals),

% test 6
[land], [agent].
init_world,
Goals = [goal(4,3), goal(4,2)],
Intentions = intents([[goal(5,3),[]],[goal(7,6),[]],[goal(8,6),[]],[goal(9,9),[]]],[]),
incorporate_goals(Goals, Intentions, Intentions1),
writeln(Intentions1).

% test 7
Intentions1 = intents([[goal(5,3),[]],[goal(7,6),[]],[goal(8,6),[]],[goal(9,9),[]]],[]),
get_action(Intentions1, Intentions2, Action).

% test 8
[land], [agent].
init_world,
Percepts = [stone(2,1)],
write('  Percepts: '), writeln(Percepts),
trigger(Percepts, Goals),
write('  Goals: '), writeln(Goals),
incorporate_goals(Goals, Intentions, Intentions1),
write('  Intentions: '), writeln(Intentions1),
get_action(Intentions1, Intentions2, Action),
write('  New Intentions: '), writeln(Intentions2),
write('  Action: '), writeln(Action), !.
