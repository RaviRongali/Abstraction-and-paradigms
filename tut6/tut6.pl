
mylength([],0).
mylength([X|XS], N) :- mylength(XS,N1), N is N1 + 1.



not_adjacent(X,Y) :- Z is X - Y, \==(Z, 1), \==(Z, -1).

occupancy([[sanket,A],[ankush,B],[ashwin,C],[umang,D],[krishna,E]]) :-
    member(C,[1,2,3,4]),
    member(B,[2,3,4,5]), \==(B,C),
    member(A,[1,2,3,4,5]), A > B, \==(A,C),
    member(D,[2,3,4]), not_adjacent(D,B), \==(D,A), \==(D,B), \==(D,C),   
    member(E,[1,2,3,4,5]), not_adjacent(D, E), \==(E,A), \==(E,B),
                                               \==(E,C), \==(E,D). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choose([A,B,C]) :- member(A,[r,y,g]), member(B,[r,y,g]), member(C,[r,y,g]),
		   count(r, [A,B,C], X), X < 3,
		   count(y, [A,B,C], Y), Y < 3,
		   count(g, [A,B,C], Z), Z < 4.

count(Color, [], 0).
count(Color, [Color|T], N) :- count(Color, T, N1), N is N1+1,!.
count(Color, [_|T], N) :- count(T, N).


goal(C, Comb) :- choose(Comb),
		 a_stmt(C, Comb),
		 b_stmt(C, Comb).

a_stmt(C, Comb) :- =([_,_,C], Comb),
		   not(=([_,y,y], Comb)),
		   not(=([_,r,r], Comb)).
		   

b_stmt(C, Comb) :- =([_,_,C], Comb),
		   not(=(Comb,[_,_,y])),
		   not(=(Comb, [_,_,r])).
		   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

within_board(A, B, C, D) :- 8 >= A,  8 >= B,  8 >= C,  8 >= D,
                            A >= 1,  B >= 1,  C >= 1, D >= 1. 
move(pair(X,Y), pair(X1,Y1)) :- X1 is X+1, Y1 is Y+2, within_board(X,Y,X1,Y1).
move(pair(X,Y), pair(X1,Y1)) :- X1 is X+1, Y1 is Y-2, within_board(X,Y,X1,Y1).

move(pair(X,Y), pair(X1,Y1)) :- X1 is X+2, Y1 is Y+1, within_board(X,Y,X1,Y1).
move(pair(X,Y), pair(X1,Y1)) :- X1 is X+2, Y1 is Y-1, within_board(X,Y,X1,Y1).

move(pair(X,Y), pair(X1,Y1)) :- X1 is X-1, Y1 is Y-2, within_board(X,Y,X1,Y1).
move(pair(X,Y), pair(X1,Y1)) :- X1 is X-1, Y1 is Y+2, within_board(X,Y,X1,Y1).

move(pair(X,Y), pair(X1,Y1)) :- X1 is X-2, Y1 is Y+1, within_board(X,Y,X1,Y1).
move(pair(X,Y), pair(X1,Y1)) :- X1 is X-2, Y1 is Y-1, within_board(X,Y,X1,Y1).


% Res is a partial Knight's tour from Curr that does not 
% step into any place in Done 

%% tour_guide(_, Done, []) :- length(Done, 49).
%% tour_guide(Curr, Done, Res) :- move(Curr, Next), not(member(Next, Done)),
%%   			       tour_guide(Next, [Curr|Done], Res1),
%%   			       =(Res, [Curr|Res1]).

tour_guide(_, Done, Done) :- length(Done, 49),!. 
tour_guide(Curr, Done, Res) :- move(Curr, Next), not(member(Next, Done)), 
  			       tour_guide(Next, [Next|Done], Res).
  			       

tour(Res) :- tour_guide(pair(1,1), [pair(1,1)], Res),
	     reverse(Res, W),
	     write(W).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
boat(1,1).
boat(0,2).
boat(2,0).
boat(1,0).
boat(0,1).

safe(0,_) :- !.                        % good example of a cut
safe(M, C) :- M >= C.

state([0, 0, right], Completed, [[0,0,right]|Completed]) :- !.
state([ML, CL, left], Completed, Journeys) :- 
	boat(M,C), 
	ML1 is ML - M, CL1 is CL - C, 
	ML1 >= 0, CL1 >= 0, 
        MR1 is 3 - ML + M, CR1 is 3 - CL + C, 
	safe(ML1, CL1), %write([ML1, CL1]), 
	safe(MR1, CR1), %write([MR1, CR1]),
	not(member([ML1, CL1, right], Completed)), 
        state([ML1, CL1, right], [[ML1, CL1, right] | Completed], Journeys). 
state([ML, CL, right], Completed, Journeys) :- 
	boat(M,C), 
	ML1 is ML + M, CL1 is CL + C, 
        MR1 is 3 - ML - M, CR1 is 3 - CL - C, 
        MR1 >= 0, CR1 >= 0,
	safe(ML1, CL1), safe(MR1, CR1),
	not(member([ML1, CL1, left], Completed)), 
        state([ML1, CL1, left],  [[ML1, CL1, left] | Completed], Journeys).
safe(Ans) :- 
        state([3,3,left], [[3,3,left]], Journeys), reverse(Journeys, Ans), 
        write(Ans).




% There are five consecutive houses, each of a different
% color and inhabited by men of different nationalities. They each
% own a different pet, have a different favorite drink and drive a
% different vehicle.
%   1. The Englishman lives in the red house.
%   2. The Spaniard owns a dog.
%   3. Coffee is drunk in the green house.
%   4. The Ukrainian drinks tea.
%   5. The green house is immediately to the right of the ivory
%      house.
%   6. The motor cycle owner keeps snails.
%   7. Bike is driven by the man who lives in the yellow
%      house.
%   8. Milk is drunk in the middle house.
%   9. The Norwegian lives in the first house on the left.
%  10. The man rides a skateboard lives in the house next to the man
%      who owns a  fox.
%  11. The man who rides a bike lives next to  the man who owns a horse.

%  12. The man with the boat drinks orange juice.
%  13. The Japanese has a car
%  14. The Norwegian lives next to the blue house.

%Who owns the zebra, who drinks water.


% left_right(L,R,List) means L is to the left of R in the list List.
left_right(L,R,[L,R,_,_,_]).
left_right(L,R,[_,L,R,_,_]).
left_right(L,R,[_,_,L,R,_]).
left_right(L,R,[_,_,_,L,R]).

% next_to(A,B,L) means A is next to B in the List L.
    
next_to(A,B,L) :- left_right(A,B,L).
next_to(A,B,L) :- left_right(B,A,L).

arrangement(S) :- S = [[_,norwegian,_,_,_],[blue,_,_,_,_],[_,_,_,milk,_],_,_],
		  member([red, englishman,_,_,_],S),
		  member([_, spaniard,_,_,dog],S),
		  member([green, _,_,coffee,_],S),
		  member([_, ukrainian,_,tea,_],S),
		  left_right([ivory,_,_,_,_],[green,_,_,_,_],S),
		  member([_,_,motorcycle,_,snails], S),
                  member([yellow,_,bike,_,_], S),
		  next_to([_,_,skateboard,_,_],[_,_,_,_,fox],S),
		  next_to([_,_,bike,_,_],[_,_,_,_,horse],S),
		  member([_,_,boat,orange_juice,_],S),
		  member([_,japanese,car,_,_],S),
		  member([_,_,_,_,zebra], S),
		  member([_,_,_,water,_], S),!.
		  
		  
goal1(Who,Who1) :- arrangement(S), member([_,Who,_,_,zebra], S),
		   member([_,Who1,_,water,_], S).

% A prolog program which changes the confiiguration
% [p,p,e,d,d] to [d,d,e,p,p].



validmove([e,X,Y,Z,W], [X,e,Y,Z,W]).
validmove([e,X,Y,Z,W], [Y,X,e,Z,W]).

validmove([X,e,Y,Z,W], [e,X,Y,Z,W]).
validmove([X,e,Y,Z,W], [X,Y,e,Z,W]).
validmove([X,e,Y,Z,W], [X,Z,Y,e,W]).

validmove([X,Y,e,Z,W], [X,e,Y,Z,W]).
validmove([X,Y,e,Z,W], [X,Y,Z,e,W]).
validmove([X,Y,e,Z,W], [e,Y,X,Z,W]).
validmove([X,Y,e,Z,W], [X,Y,W,Z,e]).

validmove([X,Y,Z,e,W], [X,Y,e,Z,W]).
validmove([X,Y,Z,e,W], [X,Y,Z,W,e]).
validmove([X,Y,Z,e,W], [X,e,Z,Y,W]).

validmove([X,Y,Z,W,e], [X,Y,Z,e,W]).
validmove([X,Y,Z,W,e], [X,Y,e,W,Z]).


arrangement([d,d,e,p,p], S, S).



arrangement([X,Y,e,Z,W],S, T) :- validmove([X,Y,e,Z,W],M), not(member(M,S)),
				 S1 = [M|S],
				 arrangement(M,S1,T).

arrangement([X,e,Y,Z,W],S,T) :- validmove([X,e,Y,Z,W],M), not(member(M,S)),
				S1 = [M|S],
				arrangement(M,S1,T).

arrangement([e,X,Y,Z,W],S,T) :- validmove([e,X,Y,Z,W],M), not(member(M,S)),
				S1 = [M|S],
				arrangement(M,S1,T).

arrangement([X,Y,Z,e,W],S,T) :- validmove([X,Y,Z,e,W],M), not(member(M,S)),
				S1 = [M|S],
				arrangement(M,S1,T).

arrangement([X,Y,Z,W,e],S,T) :- validmove([X,Y,Z,W,e],M), not(member(M,S)),
				S1 = [M|S],
				arrangement(M,S1,T).
			      
%arrangement([p,p,e,d,d], [[p,p,e,d,d]], T), reverse(T, T1), write(T1).

