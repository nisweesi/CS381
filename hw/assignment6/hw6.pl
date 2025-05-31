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
