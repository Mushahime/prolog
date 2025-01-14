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
    asserta( board([[E,E,E,E,E,E,E], [E,E,E,E,E,E,E], [E,E,E,E,E,E,E], [E,E,E,E,E,E,E], [E,E,E,E,E,E,E], [E,E,E,E,E,E,E]]) )  %%% create a blank board
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

% Affiche une ligne de la matrice
output_line([]) :- nl. % Quand la ligne est vide, saute une ligne.
output_line([X|Xs]) :-
    write(X), % Affiche le contenu de la case (X, O, ou _).
    write('|'), % Ajoute un séparateur vertical.
    output_line(Xs). % Continue avec le reste de la ligne.

% Affiche tout le plateau
output_board([]). % Quand il n y a plus de lignes termine
output_board([Row|Rows]) :-
    output_line(Row), % Affiche la ligne courante.
    write('---------------------'), nl, % Ligne de séparation (6 colonnes).
    output_board(Rows). % Continue avec les autres lignes.


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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PSEUDO-RANDOM NUMBERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% random_seed
%.......................................
% Initialize the random number generator...
% If no seed is provided, use the current time
%

random_seed :-
    random_seed(_),
    !
    .

random_seed(N) :-
    nonvar(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

random_seed(N) :-
    var(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_seed(N) :-
    nonvar(N),
    randomize(N),
    !
    .

arity_prolog___random_seed(N) :-
    var(N),
    time(time(Hour,Minute,Second,Tick)),
    N is ( (Hour+1) * (Minute+1) * (Second+1) * (Tick+1)),
    randomize(N),
    !
    .

******************************************/



%.......................................
% random_int_1n
%.......................................
% returns a random integer from 1 to N
%
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_int_1n(N, V) :-
    R is random,
    V2 is (R * N) - 0.5,
    float_text(V2,V3,fixed(0)),
    int_text(V4,V3),
    V is V4 + 1,
    !
    .

******************************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LIST PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member([V|T], V).
member([_|T], V) :- member(T,V).

append([], L, L).
append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).


%.......................................
% set_item
%.......................................
% Given a list L, replace the item at position N with V
% return the new list in list L2
%

set_item(L, N, V, L2) :-
    set_item2(L, N, V, 1, L2)
        .

set_item2( [], N, V, A, L2) :-
    N == -1,
    L2 = []
    .

set_item2( [_|T1], N, V, A, [V|T2] ) :-
    A = N,
    A1 is N + 1,
    set_item2( T1, -1, V, A1, T2 )
    .

set_item2( [H|T1], N, V, A, [H|T2] ) :-
    A1 is A + 1,
    set_item2( T1, N, V, A1, T2 )
    .


%.......................................
% get_item
%.......................................
% Given a list L, retrieve the item at position N and return it as value V
%

get_item(L, N, V) :-
    get_item2(L, N, 1, V)
    .

get_item2( [], _N, _A, V) :-
    V = [], !,
    fail
        .

get_item2( [H|_T], N, A, V) :-
    A = N,
    V = H
    .

get_item2( [_|T], N, A, V) :-
    A1 is A + 1,
    get_item2( T, N, A1, V)
    .




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WIN CONDITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Check for a win (horizontal, vertical, or diagonal)
win(Board, Player) :-
    horizontal_win(Board, Player);
    vertical_win(Board, Player);
    diagonal_win(Board, Player).

% Check for a horizontal win
horizontal_win(Board, Player) :-
    member(Row, Board), % Check each row in the board
    sublist([Player, Player, Player, Player], Row).

% Check for a vertical win
vertical_win(Board, Player) :-
    transpose(Board, TransposedBoard), % Transpose the board to check columns
    horizontal_win(TransposedBoard, Player).

% Check for diagonal wins
diagonal_win(Board, Player) :-
    diagonal_win_right(Board, Player);
    diagonal_win_left(Board, Player).

% Check diagonal going down-right
diagonal_win_right(Board, Player) :-
    append(_, [Row1, Row2, Row3, Row4|_], Board),
    append(Prefix1, [Player|_], Row1),
    append(Prefix2, [Player|_], Row2),
    append(Prefix3, [Player|_], Row3),
    append(Prefix4, [Player|_], Row4),
    length(Prefix1, N1),
    length(Prefix2, N2),
    length(Prefix3, N3),
    length(Prefix4, N4),
    N2 is N1 + 1,
    N3 is N2 + 1,
    N4 is N3 + 1.

% Check diagonal going down-left
diagonal_win_left(Board, Player) :-
    append(_, [Row1, Row2, Row3, Row4|_], Board),
    append(Prefix1, [Player|_], Row1),
    append(Prefix2, [Player|_], Row2),
    append(Prefix3, [Player|_], Row3),
    append(Prefix4, [Player|_], Row4),
    length(Prefix1, N1),
    length(Prefix2, N2),
    length(Prefix3, N3),
    length(Prefix4, N4),
    N2 is N1 - 1,
    N3 is N2 - 1,
    N4 is N3 - 1.

% Check if a list contains a sublist
sublist(Sublist, List) :-
    append(_, Suffix, List), % Slice the List to get the suffix
    append(Sublist, _, Suffix). % Check if Sublist is at the start of the suffix

% Transpose a matrix (turn rows into columns)
transpose([], []).
transpose([[]|_], []).
transpose(Matrix, [Col|Cols]) :-
    first_column(Matrix, Col, RestMatrix),
    transpose(RestMatrix, Cols).

% Helper predicate to get the first column of a matrix
first_column([], [], []).
first_column([[H|T]|Rows], [H|Heads], [T|RestRows]) :-
    first_column(Rows, Heads, RestRows).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

