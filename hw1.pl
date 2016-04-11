%% Hayden Platt - HW1

%% Family Data for Questions 9-11
father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

%%9 - Predicate to determine if X is the grandmother of Y
grandma(X,Y) :- mother(X,Z), mother(Z, Y).

%%10 - Predicate to determine if X is a direct or indirect
%% descendant of Y
descendants(X,Y) :- 
    mother(X,Y);
    father(X,Y);
    mother(X,Z), mother(Z,Y);
    father(X,Z), father(Z,Y).

%%11 - Predicate to determine if X and Y have a common parent,
%% but are not the same person.
siblings(X,Y) :- 
    mother(Z,X), mother(Z,Y), X \= Y; 
    father(Z,X), father(Z,Y), X \= Y.

	
%% Transition Data for Question 12
transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).
accepting(q2).
accepting(q3).

%%12 - Function which determines the possible outcomes of the above
%% NFA
accepts(State, InputList) :- 
    InputList = [H|T],
    transition(State, NextState, H),
    accepts(NextState, T);
    InputList = [],
    accepting(State).