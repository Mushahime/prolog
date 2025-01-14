%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CST 381 -� Artificial Intelligence
%%% Robert Pinchbeck
%%% Final Project
%%% Due December 20, 2006
%%% Source : http://www.robertpinchbeck.com/college/work/prolog/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A Prolog Implementation of Tic-Tac-Toe
%%% using the minimax strategy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

The following conventions are used in this program...

Single letter variables represent:

L - a list
N - a number, position, index, or counter
V - a value (usually a string)
A - an accumulator
H - the head of a list
T - the tail of a list

For this implementation, these single letter variables represent:

P - a player number (1 or 2)
B - the board (a 9 item list representing a 3x3 matrix)
    each "square" on the board can contain one of 3 values: x ,o, or e (for empty)
S - the number of a square on the board (1 - 9)
M - a mark on a square (x or o)
E - the mark used to represent an empty square ('e').
U - the utility value of a board position
R - a random number
D - the depth of the minimax search tree (for outputting utility values, and for debugging)

Variables with a numeric suffix represent a variable based on another variable.
(e.g. B2 is a new board position based on B)

For predicates, the last variable is usually the "return" value.
(e.g. opponent_mark(P,M), returns the opposing mark in variable M)

Predicates with a numeric suffix represent a "nested" predicate.

e.g. myrule2(...) is meant to be called from myrule(...)
     and myrule3(...) is meant to be called from myrule2(...)


There are only two assertions that are used in this implementation

asserta( board(B) ) - the current board
asserta( player(P, Type) ) - indicates which players are human/computer.

*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark('x', 'o'). %%% determines the opposite of the given mark
inverse_mark('o', 'x').

player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

blank_mark('e').        %%% the mark used in an empty square

maximizing('x').        %%% the player playing x is always trying to maximize the utility of the board position
minimizing('o').        %%% the player playing o is always trying to minimize the utility of the board position

corner_square(1, 1).    %%% map corner squares to board squares
corner_square(2, 7).
corner_square(3, 36).
corner_square(4, 42).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run :-
    hello,          %%% Display welcome message, initialize game

    play(1),        %%% Play the game starting with player 1

    goodbye         %%% Display end of game message
    .

run :-
    goodbye
    .


hello :-
    initialize,
%    cls,
    nl,
    nl,
    nl,
    write('Welcome to Connect 4.'),
    read_players,
    output_players
    .

initialize :-
    random_seed,          %%% use current time to initialize random number generator
    blank_mark(E),
    asserta( board([E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E]) )  %%% create a blank board
    .

goodbye :-
    board(B),
    nl,
    nl,
    write('Game over: '),
    output_winner(B),
    retract(board(_)),
    retract(player(_,_)),
    read_play_again(V), !,
    (V == 'Y' ; V == 'y'),
    !,
    run
    .

read_play_again(V) :-
    nl,
    nl,
    write('Play again (Y/N)? '),
    read(V),
    (V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !
    .

read_play_again(V) :-
    nl,
    nl,
    write('Please enter Y or N.'),
    read_play_again(V)
    .


read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    .

set_players(0) :-
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !
    .

set_players(1) :-
    nl,
    write('Is human playing X or O (X moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :-
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

set_players(N) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .


human_playing(M) :-
    (M == 'x' ; M == 'X'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

human_playing(M) :-
    (M == 'o' ; M == 'O'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !
    .

human_playing(M) :-
    nl,
    write('Please enter X or O.'),
    set_players(1)
    .


play(P) :-
    board(B), !,
    output_board(B), !,
    not(game_over(P, B)), !,
    make_move(P, B), !,
    next_player(P, P2), !,
    play(P2), !
    .



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :-
    nl,
    player(1, V1),
    write('Player 1 is '),   %%% either human or computer
    write(V1),

    nl,
    player(2, V2),
    write('Player 2 is '),   %%% either human or computer
    write(V2),
    !
    .


output_winner(B) :-
    win(B,x),
    write('X wins.'),
    !
    .

output_winner(B) :-
    win(B,o),
    write('O wins.'),
    !
    .

output_winner(B) :-
    write('No winner.')
    .


output_board(B) :-
    nl,
    nl,
    output_square(B,1),
    write('|'),
    output_square(B,2),
    write('|'),
    output_square(B,3),
    write('|'),
    output_square(B,4),
    write('|'),
    output_square(B,5),
    write('|'),
    output_square(B,6),
    write('|'),
    output_square(B,7),
    nl,
    write('-----------'),
    nl,
    output_square(B,8),
    write('|'),
    output_square(B,9),
    write('|'),
    output_square(B,10),
    write('|'),
    output_square(B,11),
    write('|'),
    output_square(B,12),
    write('|'),
    output_square(B,13),
    write('|'),
    output_square(B,14),
    nl,
    write('-----------'),
    nl,
    output_square(B,15),
    write('|'),
    output_square(B,16),
    write('|'),
    output_square(B,17),
    write('|'),
    output_square(B,18),
    write('|'),
    output_square(B,19),
    write('|'),
    output_square(B,20),
    write('|'),
    output_square(B,21),
    nl,
    write('-----------'),
    nl,
    output_square(B,22),
    write('|'),
    output_square(B,23),
    write('|'),
    output_square(B,24),
    write('|'),
    output_square(B,25),
    write('|'),
    output_square(B,26),
    write('|'),
    output_square(B,27),
    write('|'),
    output_square(B,28),
    nl,
    write('-----------'),
    nl,
    output_square(B,29),
    write('|'),
    output_square(B,30),
    write('|'),
    output_square(B,31),
    write('|'),
    output_square(B,32),
    write('|'),
    output_square(B,33),
    write('|'),
    output_square(B,34),
    write('|'),
    output_square(B,35),
    nl,
    write('-----------'),
    nl,
    output_square(B,36),
    write('|'),
    output_square(B,37),
    write('|'),
    output_square(B,38),
    write('|'),
    output_square(B,39),
    write('|'),
    output_square(B,40),
    write('|'),
    output_square(B,41),
    write('|'),
    output_square(B,42),!
    .

output_board :-
    board(B),
    output_board(B), !
    .

output_square(B,S) :-
    square(B,S,M),
    write(' '),
    output_square2(S,M),
    write(' '), !
    .

output_square2(S, E) :-
    blank_mark(E),
    write(S), !              %%% if square is empty, output the square number
    .

output_square2(S, M) :-
    write(M), !              %%% if square is marked, output the mark
    .

output_value(D,S,U) :-
    D == 1,
    nl,
    write('Square '),
    write(S),
    write(', utility: '),
    write(U), !
    .

output_value(D,S,U) :-
    true
    .