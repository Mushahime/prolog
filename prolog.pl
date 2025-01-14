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
    write('Welcome to Tic-Tac-Toe.'),
    read_players,
    output_players
    .

initialize :-
    random_seed,          %%% use current time to initialize random number generator
    blank_mark(E),
    asserta( board([E,E,E, E,E,E, E,E,E]) )  %%% create a blank board
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




