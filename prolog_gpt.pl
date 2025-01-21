% Basic facts

write_red(X) :- write('\e[31m'), write(X), write('\e[0m').
write_yellow(X) :- write('\e[33m'), write(X), write('\e[0m').

next_player(1, 2).
next_player(2, 1).

inverse_mark('R', 'Y').
inverse_mark('Y', 'R').

player_mark(1, 'R').
player_mark(2, 'Y').

opponent_mark(1, 'Y').
opponent_mark(2, 'R').

blank_mark('_').

maximizing('R').
minimizing('Y').

% Main program
run :-
    initialize,
    nl, write('Welcome to Connect 4!'), nl,
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
    write('Board initialized: '), nl,
    display_board(Board).

% Player configuration remains the same
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
    read_players
    .

human_playing(M) :-
    retractall(player(_,_)),
    (M == 'r'),
        asserta(player(1, human)),
        asserta(player(2, computer)), !.

human_playing(M) :-
    retractall(player(_,_)),
    (M == 'y'),
    asserta(player(1, computer)),
    asserta(player(2, human)), !.

human_playing(_) :-
    nl,
    write('Please enter r/R or y/Y.'),
    set_players(1).

% Improved board display
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

display_cell(X) :-
    (X = 'R' -> write_red(X) ;
     X = 'Y' -> write_yellow(X) ;
     write(X)).

display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell), write(' '),
    display_row(Rest).

play(P) :-
    board(B),
    display_board(B),
    write(P), write('\'s turn'), nl,
    make_move(P),
    board(NewB),
    (contains_mark(NewB) ->  % Vérifie si le plateau contient au moins une marque de joueur
        (check_win(NewB, P) ->
            display_board(NewB),  % Display the final board
            write('Player '), write(P), write(' wins!'), nl,
            write('Play again? (y/n): '),
            read(Answer),
            (Answer == 'y' -> replay(P) ; true)
        ; board_full(NewB) ->
            write('Game is a draw!'), nl,
            display_board(NewB),  % Display the final board
            write('Play again? (y/n): '),
            read(Answer),
            (Answer == 'y' -> replay(P) ; true)
        ;
            next_player(P, P2),
            play(P2)
        )
    ; next_player(P, P2),
      play(P2)
    ).

contains_mark(Board) :-
    player_mark(_, Mark),
    member(Row, Board),
    member(Mark, Row), !.

replay(P) :-
    initialize,
    read_players,
    play(P).

% Move management
make_move(P) :-
    player(P, Type),
    player_mark(P, Mark),
    (Type = human ->
        write('Player '), write(P),
        write(' (column 1-7): '),
        read(Col),
        make_human_move(Col, Mark)
    ;
        write('Computer is thinking...'), nl,
        choose_computer_move(Mark, Col),
        make_computer_move(Col, Mark)
    ).

% Move validation and execution
valid_move(Col) :-
    integer(Col),
    Col >= 1, Col =< 7,
    board(B),
    nth1(1, B, TopRow),
    nth1(Col, TopRow, Cell),
    blank_mark(E),
    Cell = E.

make_human_move(Col, Mark) :-
    valid_move(Col),
    board(B),
    drop_piece(B, Col, Mark, NewBoard),
    retract(board(_)),
    asserta(board(NewBoard)),
    write('Human plays column '), write(Col), nl, !.

make_computer_move(Col, Mark) :-
    valid_move(Col),
    board(B),
    drop_piece(B, Col, Mark, NewBoard),
    retract(board(_)),
    asserta(board(NewBoard)),
    write('Computer plays column '), write(Col), nl, !.

% Fixed piece dropping function
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

% Helper to replace element in nested list
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

% Check if the board is full
board_full(Board) :-
    blank_mark(E),
    forall(member(Row, Board),
           forall(member(Cell, Row),
                  Cell \= E)).

% Win checking
check_win(Board, P) :-
    player_mark(P, Mark),
    (check_horizontal_win(Board, Mark) ;
     check_vertical_win(Board, Mark) ;
     check_diagonal_win(Board, Mark)).

% Horizontal win check
check_horizontal_win(Board, Mark) :-
    member(Row, Board),
    four_consecutive(Row, Mark).

% Vertical win check
check_vertical_win(Board, Mark) :-
    transpose_board(Board, Transposed),
    member(Col, Transposed),
    four_consecutive(Col, Mark).

% Check for diagonal wins
check_diagonal_win(Board, Mark) :-
    diagonal_win_right(Board, Mark);
    diagonal_win_left(Board, Mark).

% Check diagonal going down-right
diagonal_win_right(Board, Mark) :-
    append(_, [Row1, Row2, Row3, Row4|_], Board),
    append(Prefix1, [Mark|_], Row1),
    append(Prefix2, [Mark|_], Row2),
    append(Prefix3, [Mark|_], Row3),
    append(Prefix4, [Mark|_], Row4),
    length(Prefix1, N1),
    length(Prefix2, N2),
    length(Prefix3, N3),
    length(Prefix4, N4),
    N2 is N1 + 1,
    N3 is N2 + 1,
    N4 is N3 + 1.

% Check diagonal going down-left
diagonal_win_left(Board, Mark) :-
    append(_, [Row1, Row2, Row3, Row4|_], Board),
    append(Prefix1, [Mark|_], Row1),
    append(Prefix2, [Mark|_], Row2),
    append(Prefix3, [Mark|_], Row3),
    append(Prefix4, [Mark|_], Row4),
    length(Prefix1, N1),
    length(Prefix2, N2),
    length(Prefix3, N3),
    length(Prefix4, N4),
    N2 is N1 - 1,
    N3 is N2 - 1,
    N4 is N3 - 1.

four_consecutive(List, Mark) :-
    append(_, [Mark,Mark,Mark,Mark|_], List).

% Board dimensions
board_height(6).
board_width(7).



% Helper to get cell safely
get_cell(Board, Row, Col, Cell) :-
    Row > 0, Row =< 6,
    Col > 0, Col =< 7,
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell).

% Check for consecutive marks
consecutive_marks(List, Mark, Count) :-
    consecutive_marks_helper(List, Mark, 0, Count).

consecutive_marks_helper([], _, Current, Required) :-
    Current >= Required.
consecutive_marks_helper([H|T], Mark, Current, Required) :-
    (H = Mark ->
        NextCount is Current + 1
    ;
        NextCount is 0
    ),
    (NextCount >= Required ->
        true
    ;
        consecutive_marks_helper(T, Mark, NextCount, Required)
    ).

four_in_a_row(List, Mark) :-
    append(_, [Mark,Mark,Mark,Mark|_], List).

% Board transposition helper
transpose_board([], []).
transpose_board([[]|_], []).
transpose_board(Matrix, [Col|Cols]) :-
    transpose_col(Matrix, Col, Rest),
    transpose_board(Rest, Cols).

transpose_col([], [], []).
transpose_col([[H|T]|Rows], [H|Cols], [T|Rest]) :-
    transpose_col(Rows, Cols, Rest).

% Simple computer move for now
choose_computer_move(_, Col) :-
    repeat,
    random_between(1, 7, Col),
    valid_move(Col), !.