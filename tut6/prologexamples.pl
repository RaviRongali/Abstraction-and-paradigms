

son(bhim, pandu).
son(yudhishthir, pandu).
son(duryodhan, dhritarashtra).

brother(pandu, dhritarashtra).
brother(dhritarashtra, pandu).

brother(X,Y) :- son(X,Z), son(Y,Z), \==(X, Y).


uncle(X,Y) :- son(Y,F), brother(F,X).

myappend([],[],[]):- !.
myappend([],X,X).
myappend([X|Y],Z,[X|V]) :- myappend(Y,Z,V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


factorial(0,1).
factorial(N,F) :- N1 is N-1, factorial(N1,F1),
		  F is N * F1.

even(0).
even(s(s(X))) :- even(X).

add(0,X,X).
add(s(X), Y, s(Z)) :- add(X,Y,Z).

remove(X, [X], []) :- !.
remove(X, [X|Y], Y).
remove(X, [A|Y], [A|Z]) :- remove(X,Y,Z).

perm([], []):-!.
perm(X, [Y|L]) :- remove(Y, X, L1), perm(L1, L).

choose(List, 1, [X] ) :- !, member(X, List).
choose(List, R , List) :- length(List, R), !.
choose([_|List], R, List1) :-  choose(List, R, List1). 
choose([X|List], R, [X|List1]) :- R1 is R - 1,
				  choose(List, R1, List1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1.f(a).
%% 2.f(b).
%% 3.g(a).
%% 4.g(b).
%% 5.h(b).
%% 6.k(X) :- f(X),g(X),h(X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

