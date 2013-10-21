/*Check or contruct a match on the grid*/
match([(X1,Y1),(X2,Y2),(X3,Y3)]) :- 
	((X2 is X1+1, X3 is X2+1, Y2 is Y1, Y3 is Y2) ; (Y2 is Y1+1, Y3 is Y2+1, X2 is X1, X3 is X2)), 		X1>0, Y1>0, X3<20, Y3<20 .

	

/*Check if a specific point on greed is a part of the shovel.*/
checkPoint(X, Y, Match1, Match2, Match3, Match4) :-
	\+member((X,Y), Match1), \+member((X,Y), Match2),\+member((X,Y), Match3),\+member((X,Y), Match4).

/*Check if any of the points given overlap with the Trash point*/
checkTrash(X1,Y1,X2,Y2,X3,Y3, Trash) :-
	((X1,Y1) = (7, 12) ; (X2,Y2) = (7, 12) ; (X3,Y3) = (7, 12)) -> Trash = trashTrue;
	Trash = trashFalse.
	
/*	Check if matches are placed in such way that they form a shovel
	Trash is placed at (7,12) point. Examples of Positions
	Initial:
	([[(6,10),(7,10),(8,10)],[(7,7),(7,8),(7,9)],[(6,11),(6,12),(6,13)],[(8,11),(8,12),(8,13)]]).
	Middle:
	([[(7,7),(7,8),(7,9)],[(5,10),(6,10),(7,10)],[(6,11),(6,12),(6,13)],[(8,11),(8,12),(8,13)]]).
	Final:
	([[(7,7),(7,8),(7,9)],[(5,10),(6,10),(7,10)],[(6,11),(6,12),(6,13)],[(5,7),(5,8),(5,9)]]).
*/
shovel([[(X11,Y11),(X21,Y21),(X31,Y31)],[(X12,Y12),(X22,Y22),(X32,Y32)], [(X13,Y13),(X23,Y23),(X33,Y33)], [(X14,Y14),(X24,Y24),(X34,Y34)]], Trash) :- 
	/*Check There are four different matches*/
	Match1 = [(X11,Y11),(X21,Y21),(X31,Y31)], match(Match1),
	Match2 = [(X12,Y12),(X22,Y22),(X32,Y32)], match(Match2),
	Match3 = [(X13,Y13),(X23,Y23),(X33,Y33)], match(Match3),
	Match4 = [(X14,Y14),(X24,Y24),(X34,Y34)], match(Match4),
	Match1 \= Match2, Match1 \= Match3, Match1\=Match4, Match2 \= Match3, Match2\=Match4, Match3\=Match4,
	/*Check that matches have no common point*/
	\+member((X11,Y11), Match2), \+member((X11,Y11), Match3), \+member((X11,Y11), Match4), 
	\+member((X21,Y21), Match2), \+member((X21,Y21), Match3), \+member((X21,Y21), Match4), 
	\+member((X31,Y31), Match2), \+member((X31,Y31), Match3), \+member((X31,Y31), Match4), 
	\+member((X12,Y12), Match1), \+member((X12,Y12), Match3), \+member((X12,Y12), Match4), 
	\+member((X22,Y22), Match1), \+member((X22,Y22), Match3), \+member((X22,Y22), Match4), 
	\+member((X32,Y32), Match1), \+member((X32,Y32), Match3), \+member((X32,Y32), Match4), 
	\+member((X13,Y13), Match1), \+member((X13,Y13), Match2), \+member((X13,Y13), Match4), 
	\+member((X23,Y23), Match1), \+member((X23,Y23), Match2), \+member((X23,Y23), Match4), 
	\+member((X33,Y33), Match1), \+member((X33,Y33), Match2), \+member((X33,Y33), Match4), 
	\+member((X14,Y14), Match1), \+member((X14,Y14), Match2), \+member((X14,Y14), Match3), 
	\+member((X24,Y24), Match1), \+member((X24,Y24), Match2), \+member((X24,Y24), Match3), 
	\+member((X34,Y34), Match1), \+member((X34,Y34), Match2), \+member((X34,Y34), Match3), 
	/*Find the minimum and maximum distance between the matches*/
	max_list([X11,X21,X31,X12,X22,X32,X13,X23,X33,X14,X24,X34], MaxX),
	max_list([Y11,Y21,Y31,Y12,Y22,Y32,Y13,Y23,Y33,Y14,Y24,Y34], MaxY),
	min_list([X11,X21,X31,X12,X22,X32,X13,X23,X33,X14,X24,X34], MinX), 
	min_list([Y11,Y21,Y31,Y12,Y22,Y32,Y13,Y23,Y33,Y14,Y24,Y34], MinY),
	MaxX - MinX =:= 2, MaxY - MinY =:= 6,
	/*Check the position*/
	((
	CheckX1 is MinX, CheckY1 is MinY, checkPoint(CheckX1,CheckY1,Match1,Match2,Match3,Match4),
	CheckX2 is MinX, CheckY2 is MinY+1, checkPoint(CheckX2,CheckY2,Match1,Match2,Match3,Match4),
	CheckX3 is MinX, CheckY3 is MinY+2, checkPoint(CheckX3,CheckY3,Match1,Match2,Match3,Match4),
	CheckX4 is MinX+2, CheckY4 is MinY, checkPoint(CheckX4,CheckY4,Match1,Match2,Match3,Match4),
	CheckX5 is MinX+2, CheckY5 is MinY+1, checkPoint(CheckX5,CheckY5,Match1,Match2,Match3,Match4),
	CheckX6 is MinX+2, CheckY6 is MinY+2, checkPoint(CheckX6,CheckY6,Match1,Match2,Match3,Match4),
	CheckX7 is MaxX-1, CheckY7 is MaxY, checkPoint(CheckX7,CheckY7,Match1,Match2,Match3,Match4),
	CheckX8 is MaxX-1, CheckY8 is MaxY-1, checkPoint(CheckX8,CheckY8,Match1,Match2,Match3,Match4),
	CheckX9 is MaxX-1, CheckY9 is MaxY-2, checkPoint(CheckX9,CheckY9,Match1,Match2,Match3,Match4),
	(((CheckX7,CheckY7) == (7, 12) ; (CheckX8,CheckY8) == (7, 12) ; (CheckX9,CheckY9) == (7, 12)) -> Trash = trashTrue;
	Trash = trashFalse)
	);
	(
	CheckX10 is MinX+1, CheckY10 is MinY, checkPoint(CheckX10,CheckY10,Match1,Match2,Match3,Match4),
	CheckX11 is MinX+1, CheckY11 is MinY+1, checkPoint(CheckX11,CheckY11,Match1,Match2,Match3,Match4),
	CheckX12 is MinX+1, CheckY12 is MinY+2, checkPoint(CheckX12,CheckY12,Match1,Match2,Match3,Match4),
	CheckX13 is MinX, CheckY13 is MaxY, checkPoint(CheckX13,CheckY13,Match1,Match2,Match3,Match4),
	CheckX14 is MinX, CheckY14 is MaxY-1, checkPoint(CheckX14,CheckY14,Match1,Match2,Match3,Match4),
	CheckX15 is MinX, CheckY15 is MaxY-2, checkPoint(CheckX15,CheckY15,Match1,Match2,Match3,Match4),
	CheckX16 is MaxX, CheckY16 is MaxY, checkPoint(CheckX16,CheckY16,Match1,Match2,Match3,Match4),
	CheckX17 is MaxX, CheckY17 is MaxY-1, checkPoint(CheckX17,CheckY17,Match1,Match2,Match3,Match4),
	CheckX18 is MaxX, CheckY18 is MaxY-2, checkPoint(CheckX18,CheckY18,Match1,Match2,Match3,Match4),
	(((CheckX10,CheckY10) == (7, 12) ; (CheckX11,CheckY11) == (7, 12) ; (CheckX12,CheckY12) == (7, 12)) -> Trash = trashTrue;
	Trash = trashFalse)
	))
	.	


finishGame(State1, State2, State3) :-
	write(State1), nl, printState(State1, 1, 1), nl, write(State2),nl, printState(State2, 1, 1),nl, write(State3), nl, printState(State3, 1, 1), fail.
constructGame(State1, State2,State3) :-
	/*Initial Positions*/
	[(X11,Y11),(X21,Y21),(X31,Y31)] = [(6,10),(7,10),(8,10)], Match1 = [(X11,Y11),(X21,Y21),(X31,Y31)],
	[(X12,Y12),(X22,Y22),(X32,Y32)] = [(7,7),(7,8),(7,9)], Match2 = [(X12,Y12),(X22,Y22),(X32,Y32)],
	[(X13,Y13),(X23,Y23),(X33,Y33)] = [(6,11),(6,12),(6,13)], Match3 = [(X13,Y13),(X23,Y23),(X33,Y33)] ,
	[(X14,Y14),(X24,Y24),(X34,Y34)] = [(8,11),(8,12),(8,13)], Match4 = [(X14,Y14),(X24,Y24),(X34,Y34)],
	shovel([Match1, Match2,Match3,Match4], Trash),
	State1 = [Match1, Match2, Match3, Match4],
		/*First Random Move*/
	random(1, 16, X1r), random(1, 20, Y1r),
	match([(X1r, Y1r), (X2r, Y2r), (X3r, Y3r)]), MatchRnd = [(X1r, Y1r), (X2r, Y2r), (X3r, Y3r)],
	RandomMatchesSet = [[MatchRnd, Match2, Match3, Match4], [Match1, MatchRnd, Match3, Match4], [Match1, Match2, MatchRnd, Match4], [Match1, Match2, Match3, MatchRnd]], 
	random(1, 5, Nr), nth(Nr, RandomMatchesSet, State2),
		/*Second Random Move*/
	nth(1, State2, Match1b), nth(2, State2, Match2b),nth(3, State2, Match3b),nth(4, State2, Match4b), 
	random(1, 16, X1rb), random(1, 20, Y1rb),
	match([(X1rb, Y1rb), (X2rb, Y2rb), (X3rb, Y3rb)]), MatchRndb = [(X1rb, Y1rb), (X2rb, Y2rb), (X3rb, Y3rb)],
	RandomMatchesSetb = [[MatchRndb, Match2b, Match3b, Match4b], [Match1b, MatchRndb, Match3b, Match4b], [Match1b, Match2b, MatchRndb, Match4b], [Match1b, Match2b, Match3b, MatchRndb]],
	random(1, 5, Nrb), nth(Nrb, RandomMatchesSetb, State3),
	/*Check if State3 forms a shovel*/
	nth(1, State3, Match1c), nth(2, State3, Match2c),nth(3, State3, Match3c),nth(4, State3, Match4c),
	(shovel([Match1c, Match2c,Match3c,Match4c], Trash2), State1 \= State3, Trash2 == trashFalse) -> finishGame(State1, State2, State3);
	constructGame(State1a, State2a,State3a).
	
	
/*Print the Grid*/
printState(State, 15, 19):-
	[Match1, Match2, Match3, Match4] = State,
	((member((15,19), Match1) ; member((15,19), Match2) ;member((15,19), Match3) ;member((15,19), Match4)) -> write('*') 
	; write('-')), !.
printState(State, X,19) :-
	[Match1, Match2, Match3, Match4] = State,
	((member((X,19), Match1) ; member((X,19), Match2) ;member((X,19), Match3) ;member((X,19), Match4)) -> write('*'), nl 
	; write('-'), nl),
	X1 is X+1,
	printState(State, X1, 1), !. 
printState(State, X, Y) :-
	[Match1, Match2, Match3, Match4] = State,
	((member((X,Y), Match1) ; member((X,Y), Match2) ;member((X,Y), Match3) ;member((X,Y), Match4)) -> write('*') 
	; (X =:= 7, Y =:=12 ) ->  write('&') 
	; write('-')),
	Y1 is Y+1, 
	printState(State, X, Y1).
	