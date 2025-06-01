/* GROUP:
    Nadir Isweesi
    William Morton
*/

% Rules/Facts

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

% Excercise 1 Database Application

% A
schedule(Student, Place, Time) :-
    enroll(Student, Class),
    where(Class, Place),
    when(Class, Time).

% B
usage(Room, Time) :-
    where(Class, Room),
    when(Class, Time).

% C
conflict(Class1, Class2) :-
    where(Class1, Place),
    where(Class2, Place),
    when(Class1, Time),
    when(Class2, Time),
    Class1 \= Class2.

% D
% Strategy is to define two predicates for both cases
meet(Student1, Student2) :-
    % Same class time and place
    schedule(Student1, Place, Time),
    schedule(Student2, Place, Time),
    Student1 \= Student2.

meet(Student1, Student2) :-
    % Back to back classes in the same place
    schedule(Student1, Place, Time1),
    schedule(Student2, Place, Time2),
    Student1 \= Student2,
    Time2 is Time1 + 1.

% Exercise 2 List Predicates and Arithmetic
% DO NOT USE flatten and nth, you can use append or member

% A
% Base cases
rdup([], []).
rdup([X], [X]).
% Recursion
rdup([X,X|T], M) :- 
    rdup([X|T], M).
rdup([X,Y|T], [X|M]) :- 
    X \= Y, 
    rdup([Y|T], M).

% B
flat([], []).
flat(X, [X]) :- X \= [], \+ is_list(X).
flat([H|T], F) :-
    flat(H, FH),
    flat(T, FT),
    append(FH, FT, F).

% C
% Helper func
lookup(1, [H|_], H).
lookup(N, [_|T], Elem) :-
    N > 1,
    N1 is N - 1,
    lookup(N1, T, Elem).

% project(PosList, ElemList, ResultList)
project([], _, []).
project([P|Ps], List, Result) :-
    lookup(P, List, E),         
    !,                          
    project(Ps, List, Rest),
    Result = [E|Rest].
project([_|Ps], List, Result) :-
    project(Ps, List, Result).
