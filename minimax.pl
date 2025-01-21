% Basic facts and utilities
write_red(X) :- write('\e[31m'), write(X), write('\e[0m').
write_yellow(X) :- write('\e[33m'), write(X), write('\e[0m').

next_player(1, 2).
next_player(2, 1).

player_mark(1, 'R').
player_mark(2, 'Y').

opponent_mark('R', 'Y').
opponent_mark('Y', 'R').

blank_mark('_').

% Board constants
board_height(6).
board_width(7).

% Main program
run :-
    initialize,
    nl, write('Welcome to Connect Four!'), nl,
    read_players,
    play(1).

% Board initialization
initialize :-
    blank_mark(E),
    Board = [
        [E,E,E,E,E,E,E],
        [E,E,E,E,E,E,E],
        [E,E,E,E,E,E,E],
        [E,E,E,E,E,E,E],
        [E,E,E,E,E,E,E],
        [E,E,E,E,E,E,E]
    ],
    retractall(board(_)),
    asserta(board(Board)),
    nl,
    write('---------------------------------'), nl,
    write('Board initialized'), nl.

% Player configuration
read_players :-
    nl, write('Number of human players (0, 1, 2)? '),
    read(N),
    set_players(N).

set_players(0) :-
    retractall(player(_,_)),
    asserta(player(1, computer)),
    asserta(player(2, computer)), !.

set_players(1) :-
    nl, write('Play with Red or Yellow (r/y)? '),
    read(M),
    human_playing(M), !.

set_players(2) :-
    retractall(player(_,_)),
    asserta(player(1, human)),
    asserta(player(2, human)), !.

set_players(_) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players.

human_playing(r) :-
    retractall(player(_,_)),
    asserta(player(1, human)),
    asserta(player(2, computer)), !.

human_playing(y) :-
    retractall(player(_,_)),
    asserta(player(1, computer)),
    asserta(player(2, human)), !.

human_playing(_) :-
    nl,
    write('Please enter r or y.'),
    set_players(1).

% Board display
display_board(Board) :-
    nl,
    write('  1 2 3 4 5 6 7'), nl,
    display_rows(Board, 1).

display_rows([], _).
display_rows([Row|Rest], RowNum) :-
    write(RowNum), write(' '),
    display_row(Row),
    nl,
    NextRow is RowNum + 1,
    display_rows(Rest, NextRow).

display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    write(' '),
    display_row(Rest).

display_cell('R') :- write_red('R').
display_cell('Y') :- write_yellow('Y').
display_cell(X) :- write(X).

% Main game loop
play(P) :-
    board(B),
    display_board(B),
    make_move(P),
    board(NewB),
    (check_win(NewB, P) ->
        display_board(NewB),
        nl,
        player_mark(P, Mark),
        write('Player '), write(Mark), write(' wins!'),
        nl,
        ask_play_again(P)
    ; board_full(NewB) ->
        display_board(NewB),
        nl,
        write('Game is a draw!'),
        nl,
        ask_play_again(P)
    ;
        next_player(P, NextP),
        play(NextP)
    ).

ask_play_again(P) :-
    write('Play again? (y/n): '),
    read(Answer),
    (Answer = y ->
        initialize,
        read_players,
        play(1)
    ; Answer = n ->
        write('Thanks for playing!'), nl
    ;
        write('Please enter y or n.'), nl,
        ask_play_again(P)
    ).

% Move handling
make_move(P) :-
    player(P, Type),
    player_mark(P, Mark),
    nl,
    write('---------------------------------'), nl,
    (Type = human ->
        write('Player '), write(Mark),
        write(' (column 1-7): '),
        read(Col),
        make_human_move(Col, Mark)
    ;
        write('Computer is thinking...'), nl,
        choose_computer_move(Mark, Col),
        make_computer_move(Col, Mark)
    ).

% Move validation
valid_move(Col) :-
    integer(Col),
    Col >= 1, Col =< 7,
    board(B),
    nth1(1, B, TopRow),
    nth1(Col, TopRow, Cell),
    blank_mark(E),
    Cell = E.

make_human_move(Col, Mark) :-
    (valid_move(Col) ->
        board(B),
        drop_piece(B, Col, Mark, NewBoard),
        retract(board(_)),
        asserta(board(NewBoard)),
        write('Human plays column '), write(Col), nl
    ;
        write('Invalid move. Try again.'), nl,
        read(NewCol),
        make_human_move(NewCol, Mark)
    ).

make_computer_move(Col, Mark) :-
    board(B),
    drop_piece(B, Col, Mark, NewBoard),
    retract(board(_)),
    asserta(board(NewBoard)),
    write('Computer '), write(Mark), write(' plays column '), write(Col), nl.

% Computer move selection
choose_computer_move(Mark, BestCol) :-
    board(Board),
    findall(Col-Score, (
        between(1, 7, Col),
        valid_move(Col),
        calculate_move_score(Board, Col, Mark, Score)
    ), Moves),
    keysort(Moves, SortedMoves),
    reverse(SortedMoves, [(BestCol-_)|_]).

% If no moves found, choose first valid move
choose_computer_move(_, Col) :-
    between(1, 7, Col),
    valid_move(Col), !.

% Calculate score for a move
calculate_move_score(Board, Col, Mark, Score) :-
    drop_piece(Board, Col, Mark, NewBoard),
    evaluate_position(NewBoard, Mark, Score).

% Position evaluation
evaluate_position(Board, Mark, Score) :-
    check_win_potential(Board, Mark, Score1),
    opponent_mark(Mark, OppMark),
    check_win_potential(Board, OppMark, Score2),
    Score is Score1 - Score2.

% Check win potential
check_win_potential(Board, Mark, Score) :-
    (check_win(Board, Mark) -> 
        Score = 1000
    ;
        count_three_in_row(Board, Mark, Count),
        Score = Count * 10
    ).

% Count three in a row occurrences
count_three_in_row(Board, Mark, Count) :-
    findall(1, three_in_row(Board, Mark), List),
    length(List, Count).

% Check for three in a row
three_in_row(Board, Mark) :-
    (horizontal_three(Board, Mark);
     vertical_three(Board, Mark);
     diagonal_three(Board, Mark)).

% Piece dropping
drop_piece(Board, Col, Mark, NewBoard) :-
    drop_piece_helper(Board, Col, Mark, 6, NewBoard).

drop_piece_helper(Board, Col, Mark, Row, NewBoard) :-
    Row > 0,
    nth1(Row, Board, CurrentRow),
    nth1(Col, CurrentRow, Cell),
    blank_mark(E),
    (Cell = E ->
        replace_nth(Board, Row, Col, Mark, NewBoard)
    ;
        NextRow is Row - 1,
        drop_piece_helper(Board, Col, Mark, NextRow, NewBoard)
    ).

% List manipulation helpers
replace_nth(Board, RowNum, ColNum, Mark, NewBoard) :-
    nth1(RowNum, Board, Row),
    replace_in_row(Row, ColNum, Mark, NewRow),
    replace_row(Board, RowNum, NewRow, NewBoard).

replace_in_row([_|Rest], 1, Mark, [Mark|Rest]) :- !.
replace_in_row([H|Rest], N, Mark, [H|NewRest]) :-
    N > 1,
    N1 is N - 1,
    replace_in_row(Rest, N1, Mark, NewRest).

replace_row([_|Rest], 1, NewRow, [NewRow|Rest]) :- !.
replace_row([H|Rest], N, NewRow, [H|NewRest]) :-
    N > 1,
    N1 is N - 1,
    replace_row(Rest, N1, NewRow, NewRest).

% Win checking
check_win(Board, P) :-
    player_mark(P, Mark),
    (check_horizontal_win(Board, Mark);
     check_vertical_win(Board, Mark);
     check_diagonal_win(Board, Mark)).

check_horizontal_win(Board, Mark) :-
    member(Row, Board),
    append(_, [Mark,Mark,Mark,Mark|_], Row).

check_vertical_win(Board, Mark) :-
    transpose_board(Board, Transposed),
    check_horizontal_win(Transposed, Mark).

check_diagonal_win(Board, Mark) :-
    (diagonal_win_right(Board, Mark);
     diagonal_win_left(Board, Mark)).

diagonal_win_right(Board, Mark) :-
    append(_, [Row1,Row2,Row3,Row4|_], Board),
    append(Pre1, [Mark|_], Row1),
    append(Pre2, [Mark|_], Row2),
    append(Pre3, [Mark|_], Row3),
    append(Pre4, [Mark|_], Row4),
    length(Pre1, N1),
    length(Pre2, N2),
    length(Pre3, N3),
    length(Pre4, N4),
    N2 is N1 + 1,
    N3 is N2 + 1,
    N4 is N3 + 1.

diagonal_win_left(Board, Mark) :-
    append(_, [Row1,Row2,Row3,Row4|_], Board),
    append(Pre1, [Mark|_], Row1),
    append(Pre2, [Mark|_], Row2),
    append(Pre3, [Mark|_], Row3),
    append(Pre4, [Mark|_], Row4),
    length(Pre1, N1),
    length(Pre2, N2),
    length(Pre3, N3),
    length(Pre4, N4),
    N2 is N1 - 1,
    N3 is N2 - 1,
    N4 is N3 - 1.

% Board state checks
board_full(Board) :-
    blank_mark(E),
    \+ (member(Row, Board), member(E, Row)).

% Board transposition
transpose_board([], []).
transpose_board([[]|_], []).
transpose_board(Matrix, [Col|Cols]) :-
    transpose_col(Matrix, Col, Rest),
    transpose_board(Rest, Cols).

transpose_col([], [], []).
transpose_col([[H|T]|Rows], [H|Cols], [T|Rest]) :-
    transpose_col(Rows, Cols, Rest).