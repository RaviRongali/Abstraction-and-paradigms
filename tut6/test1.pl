%%if and else fuction
position("shojib",lecturer).
position('shohel','senior Lecturer').
find_position:-
        write ('whose position ?',nl,
        read(Input),nl,
        position(Input,Output),nl,
        write(Output).



%%if and else
go:- write ('give the number'),nl,
     read(X),nl,read(Y),nl,com(X,Y).
com(X,Y):-X>Y,write('X is Bigger value');X < Y,write(' Y is greater')


