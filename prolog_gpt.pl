<<<<<<< HEAD
% Basic facts

write_red(X) :- write('\e[31m'), write(X), write('\e[0m').
write_yellow(X) :- write('\e[33m'), write(X), write('\e[0m').

next_player(1, 2).
next_player(2, 1).
=======
:- dynamic board/1.
:- dynamic player/2.
>>>>>>> noam

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. Basic Facts and Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

blank_mark('_').

% We have two marks: 'R' for Red, 'Y' for Yellow
opponent_mark('R', 'Y').
opponent_mark('Y', 'R').

% Optionally, link player IDs to marks
player_mark(1, 'R').
player_mark(2, 'Y').

% If you want to set a maximum search depth for minimax:
max_depth(5).

% Preferred column ordering for AI (used in find_winning_move/block and minimax):
ordered_columns([4,3,5,2,6,1,7]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. Main Entry Point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run :-
<<<<<<< HEAD
    initialize,
    nl, write('Welcome to Connect 4 !'), nl,
=======
    initialize_board,
    nl, write('Welcome to Connect 4!'), nl,
>>>>>>> noam
    read_players,
    play(1).   % Start with player 1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3. Board Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initialize_board :-
    blank_mark(E),
    % A standard 6 x 7 board:
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
<<<<<<< HEAD
    nl,
    write('---------------------------------'), nl,
    write('Board initialized: '), nl.
=======
>>>>>>> noam

    nl, write('---------------------------------'), nl,
    write('Board initialized.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4. Player Setup: Human or Computer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_players :-
    nl, write('Number of human players (0, 1, or 2)? '),
    read(N),
<<<<<<< HEAD
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
    make_move(P),
    board(NewB),
    nl,
    (contains_mark(NewB) ->  % Vérifie si le plateau contient au moins une marque de joueur
        (check_win(NewB, P) ->
            display_board(NewB),  % Display the final board
            nl,
            write('Player '), write(P), write(' wins!'),
            nl,
            write('Play again? (y/n)'),
            read(Answer),
            (Answer == 'y' -> replay(P) ; true)
        ; board_full(NewB) ->
            write('Game is a draw!'), nl,
            display_board(NewB),  % Display the final board
            ask_play_again(P)
        ;
            next_player(P, P2),
            play(P2)
        )
    ; next_player(P, P2),
      play(P2)
    ).

ask_play_again(P) :-
    write('Play again? (y/n): '),
    read(Answer),
    (Answer == 'y' -> replay(P) 
    ; Answer == 'n' -> write('Goodbye!'), nl 
    ; write('Please enter y or n.'), nl, ask_play_again(P)).

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
    nl,
    write('---------------------------------'), nl,
    (Type = human ->
        player_mark(P, Mark),
        write('Player '), write(Mark),
        write(' (column 1-7)'),
=======
    ( N = 0 -> set_zero_players
    ; N = 1 -> set_one_player
    ; N = 2 -> set_two_players
    ; write('Please enter 0, 1, or 2.'), nl, read_players
    ).

set_zero_players :-
    retractall(player(_,_)),
    asserta(player(1, computer)),
    asserta(player(2, computer)).

set_one_player :-
    nl, write('Play with Red or Yellow? (r/y) '),
    read(Choice),
    ( Choice = r ->
        retractall(player(_,_)),
        asserta(player(1, human)),     % Human is Red
        asserta(player(2, computer))   % Computer is Yellow
      ; Choice = y ->
        retractall(player(_,_)),
        asserta(player(1, computer)),
        asserta(player(2, human))
      ; write('Please enter r or y.'), nl, set_one_player
    ).

set_two_players :-
    retractall(player(_,_)),
    asserta(player(1, human)),
    asserta(player(2, human)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5. Game Loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play(PlayerID) :-
    board(Board),
    display_board(Board),
    make_move(PlayerID),
    board(NewBoard),
    nl,
    ( contains_any_mark(NewBoard) ->
        ( check_win(NewBoard, PlayerID) ->
            display_board(NewBoard),
            nl, write('Player '), write(PlayerID), write(' wins!'), nl,
            write('Play again? (y/n) '),
            read(Answer),
            ( Answer = y -> replay
            ; true
            )
          ; board_full(NewBoard) ->
            write('Game is a draw!'), nl,
            display_board(NewBoard),
            ask_play_again
          ; next_player(PlayerID, Next),
            play(Next)
        )
      ; next_player(PlayerID, Next),
        play(Next)
    ).

% Helper to see if the board has any R or Y
contains_any_mark(Board) :-
    member(Row, Board),
    member(Mark, Row),
    Mark \= '_', !.

% For toggling between players 1 and 2
next_player(1, 2).
next_player(2, 1).

ask_play_again :-
    write('Play again? (y/n): '),
    read(Ans),
    ( Ans = y -> replay
    ; Ans = n -> write('Goodbye!'), nl
    ; write('Please enter y or n.'), nl, ask_play_again
    ).

replay :-
    initialize_board,
    read_players,
    play(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 6. Making Moves (Human vs. Computer)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_move(PlayerID) :-
    player(PlayerID, Type),
    player_mark(PlayerID, Mark),
    nl, write('---------------------------------'), nl,
    ( Type = human ->
        write('Player '), write(Mark), write(' (1-7) '),
>>>>>>> noam
        read(Col),
        make_human_move(Col, Mark)
      ; % Computer
        write('Computer is thinking...'), nl,
        sleep(1),
<<<<<<< HEAD
        choose_computer_move(Mark, Col),
        make_computer_move(Col, Mark)
=======
        choose_computer_move(Mark, BestCol),
        make_computer_move(BestCol, Mark)
>>>>>>> noam
    ).

% Human tries to place in column `Col`
make_human_move(Col, Mark) :-
    board(OldBoard),
    ( valid_move(OldBoard, Col) ->
        drop_piece(OldBoard, Col, Mark, NewBoard),
        retract(board(_)),
        asserta(board(NewBoard)),
        write('Human plays column '), write(Col), nl
      ; write('Invalid column. Try again.'), nl,
        fail
    ).

make_computer_move(Col, Mark) :-
<<<<<<< HEAD
    valid_move(Col),
    board(B),
    drop_piece(B, Col, Mark, NewBoard),
    retract(board(_)),
    asserta(board(NewBoard)),
    write('Computer '), write(Mark), write(' plays column '), write(Col), nl, !.
=======
    board(OldBoard),
    ( valid_move(OldBoard, Col) ->
        drop_piece(OldBoard, Col, Mark, NewBoard),
        retract(board(_)),
        asserta(board(NewBoard)),
        write('Computer '), write(Mark), write(' plays column '), write(Col), nl
      ; % fallback: pick next valid
        between(1,7,Alt),
        valid_move(OldBoard, Alt),
        drop_piece(OldBoard, Alt, Mark, NB),
        retract(board(_)),
        asserta(board(NB)),
        write('Computer '), write(Mark), write(' plays column '), write(Alt), nl
    ).
>>>>>>> noam

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 7. Valid Move & Dropping Pieces
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% valid_move(+Board, +Col) : top cell in that column must be blank
valid_move(Board, Col) :-
    integer(Col),
    Col >= 1, Col =< 7,
    nth1(1, Board, TopRow),
    blank_mark(B),
    nth1(Col, TopRow, B).  % top cell = '_'

% drop_piece(+Board, +Col, +Mark, -NewBoard)
% Place Mark into first blank cell from bottom
drop_piece(Board, Col, Mark, NewBoard) :-
    drop_piece_helper(Board, Col, Mark, 6, NewBoard).

drop_piece_helper(_, _, _, 0, _) :-
    !, fail.  % column is full -> fail

drop_piece_helper(Board, Col, Mark, Row, NewBoard) :-
    nth1(Row, Board, ThisRow),
    nth1(Col, ThisRow, Cell),
    blank_mark(B),
    ( Cell = B ->
        replace_nth(Board, Row, Col, Mark, NewBoard)
      ; R2 is Row - 1,
        drop_piece_helper(Board, Col, Mark, R2, NewBoard)
    ).

% Helpers to replace row/col in nested lists
replace_nth(Board, RowNum, ColNum, Mark, NewBoard) :-
    nth1(RowNum, Board, OldRow),
    replace_in_row(OldRow, ColNum, Mark, NewRow),
    replace_row(Board, RowNum, NewRow, NewBoard).

replace_in_row([_|T], 1, X, [X|T]) :- !.
replace_in_row([H|T], N, X, [H|T2]) :-
    N > 1, N2 is N-1,
    replace_in_row(T, N2, X, T2).

replace_row([_|R], 1, NewRow, [NewRow|R]) :- !.
replace_row([H|R], N, NewRow, [H|R2]) :-
    N > 1, N2 is N-1,
    replace_row(R, N2, NewRow, R2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 8. Display Board
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_board(Board) :-
    nl, write('  1 2 3 4 5 6 7'), nl,
    display_rows(Board, 1).

display_rows([], _).
display_rows([Row|Rows], N) :-
    write(N), write(' '),
    display_row(Row), nl,
    N2 is N+1,
    display_rows(Rows, N2).

display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell), write(' '),
    display_row(Rest).

display_cell('R') :- write('\e[31mR\e[0m').  % red
display_cell('Y') :- write('\e[33mY\e[0m').  % yellow
display_cell(X)   :- write(X).               % '_'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 9. Check Win
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_win(Board, PlayerID) :-
    player_mark(PlayerID, Mark),
    ( check_horizontal_win(Board, Mark)
    ; check_vertical_win(Board, Mark)
    ; check_diagonal_win(Board, Mark)
    ).

check_horizontal_win(Board, Mark) :-
    member(Row, Board),
    four_consecutive(Row, Mark).

check_vertical_win(Board, Mark) :-
    transpose_board(Board, TBoard),
    member(Col, TBoard),
    four_consecutive(Col, Mark).

<<<<<<< HEAD
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
=======
check_diagonal_win(Board, Mark) :-
    board_dimensions(Board, Rows, Cols),
    between(1, Rows, Row),
    between(1, Cols, Col),
    ( check_line(Board, Mark, Row, Col, 1, 1)   % Diagonale ↘
    ; check_line(Board, Mark, Row, Col, -1, 1)  % Diagonale ↗
    ).

% Vérifie 4 cases consécutives dans une direction (dRow, dCol)
check_line(Board, Mark, Row, Col, DRow, DCol) :-
    get_cell(Board, Row, Col, Mark),
    R2 is Row + DRow, C2 is Col + DCol,
    get_cell(Board, R2, C2, Mark),
    R3 is R2 + DRow, C3 is C2 + DCol,
    get_cell(Board, R3, C3, Mark),
    R4 is R3 + DRow, C4 is C3 + DCol,
    get_cell(Board, R4, C4, Mark).

diagonal_left(Board, Mark) :-
    append(_, [R1,R2,R3,R4|_], Board),
    append(P1,[Mark|_],R1),
    append(P2,[Mark|_],R2),
    append(P3,[Mark|_],R3),
    append(P4,[Mark|_],R4),
    length(P1,N1),
    length(P2,N2), N2 is N1-1,
    length(P3,N3), N3 is N2-1,
    length(P4,N4), N4 is N3-1.
>>>>>>> noam

four_consecutive(List, Mark) :-
    append(_, [Mark,Mark,Mark,Mark|_], List).

board_full(Board) :-
    blank_mark(B),
    \+ (member(Row, Board), member(B, Row)).

<<<<<<< HEAD

=======
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 10. Blocking and Winning Move Checks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Find an immediate winning move for Mark
find_winning_move(Board, Mark, Col) :-
    ordered_columns(Cols),
    member(Col, Cols),
    valid_move(Board, Col),
    drop_piece(Board, Col, Mark, TempBoard),
    player_mark(PID, Mark),  % We only need Mark to be known but ensure it belongs to some PID
    check_win(TempBoard, PID),
    !.

% Find a move that blocks the opponent from an immediate win
% (i.e., if the opponent can play this column next turn and win, we place our piece there now).
find_block_move(Board, OpponentMark, Col) :-
    ordered_columns(Cols),
    member(Col, Cols),
    valid_move(Board, Col),
    drop_piece(Board, Col, OpponentMark, TempBoard),
    player_mark(OppID, OpponentMark),
    check_win(TempBoard, OppID),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 11. Minimax (Negamax) AI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% choose_computer_move(+Mark, -BestCol)
choose_computer_move(Mark, BestCol) :-
    board(Board),
    opponent_mark(Mark, OppMark),

    % 1 Check if we (Mark) can win immediately:
    ( find_winning_move(Board, Mark, WinCol) ->
        BestCol = WinCol,
        write('Computer goes for the WIN in column '), write(BestCol), nl

    % 2 Otherwise, see if the opponent can win next turn, and block:
    ; find_block_move(Board, OppMark, BlockCol) ->
        BestCol = BlockCol,
        write('Computer blocks opponent by playing column '), write(BlockCol), nl

    % 3 If no immediate win or block needed, fall back to minimax:
    ; max_depth(MaxD),
      evaluate_columns(Board, Mark, [1,2,3,4,5,6,7], OrderedCols),
      Alpha is -1000000,
      Beta  is  1000000,
      write('Starting minimax computation...'), nl,
      minimax(Board, MaxD, Alpha, Beta, Mark, OrderedCols, BestCol, Score),
      write('Selected column '), write(BestCol),
      write(' with score '), write(Score), nl
    ).

% ==== zip/3 ==== (Combine deux listes en une liste de paires Score-Colonne)
zip([], [], []).
zip([S|Scores], [C|Cols], [(S,C)|Rest]) :- zip(Scores, Cols, Rest).

% ==== pairs_values/2 ==== (Extrait les colonnes de la liste de paires)
pairs_values([], []).
pairs_values([(_Score, Col)|T], [Col|Rest]) :- pairs_values(T, Rest).
>>>>>>> noam

% evaluate_columns/4
evaluate_columns(Board, Mark, Cols, OrderedCols) :-
    maplist(column_potential(Board, Mark), Cols, Scores),
    zip(Scores, Cols, Pairs),
    sort(Pairs, Sorted),
    reverse(Sorted, Descending),
    pairs_values(Descending, OrderedCols).

% Score rapide basé sur le centre et les menaces immédiates
column_potential(Board, Mark, Col, Score) :-
    ( Col =:= 4 -> Base = 10 ; Base = 0 ),  % Bonus central
    ( valid_move(Board, Col),
      drop_piece(Board, Col, Mark, TempBoard),
      check_immediate_win(TempBoard, Mark) -> Threat = 100
    ; Threat = 0
    ),
    Score is Base + Threat.

choose_computer_move(Mark, 4) :-
    board(Board),
    is_empty_board(Board),  % Vérifie si le plateau est vide
    valid_move(Board, 4), !.

% Vérifie si le plateau est entièrement vide
is_empty_board(Board) :-
    blank_mark(B),
    forall(member(Row, Board), Row = [B,B,B,B,B,B,B]).

% minimax(+Board, +Depth, +Alpha, +Beta, +Mark, +Cols, -BestCol, -BestScore)
% Negamax approach with alpha-beta pruning
minimax(Board, 0, _, _, Mark, _, -1, Score) :-
    % Base case: no depth left => evaluate
    evaluate_position(Board, Mark, Score), !.

% If there are no columns left, fail case
minimax(_, _, _, _, _, [], -1, -999999) :- !.

minimax(Board, Depth, Alpha, Beta, Mark, [Col|RestCols], BestCol, BestScore) :-
    ( make_move_temp(Board, Col, Mark, TempBoard) ->
        D1 is Depth - 1,
        opponent_mark(Mark, OppMark),
        % Flip alpha and beta for negamax
        minimax(TempBoard, D1, -Beta, -Alpha, OppMark, [4,3,5,2,6,1,7], _, ChildScore),
        ThisScore is -ChildScore,

        ( ThisScore > Beta ->
            % Beta cutoff
            BestScore = ThisScore,
            BestCol   = Col
        ; NewAlpha is max(Alpha, ThisScore),
          % Recurse among the remaining columns
          minimax(Board, Depth, NewAlpha, Beta, Mark, RestCols, TempC, TempS),
          ( ThisScore > TempS ->
                BestCol   = Col,
                BestScore = ThisScore
          ; BestCol   = TempC,
            BestScore = TempS
          )
        )
      ; % If we cannot make a move in Col, skip it
        minimax(Board, Depth, Alpha, Beta, Mark, RestCols, BestCol, BestScore)
    ).

% A helper to "simulate" making a move, returning the new board,
% without actually updating `board/1`.
make_move_temp(Board, Col, Mark, NewBoard) :-
    valid_move(Board, Col),
    drop_piece(Board, Col, Mark, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 12. Position Evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evaluate_position/3
evaluate_position(Board, Mark, Score) :-
    opponent_mark(Mark, OppMark),
    ( check_immediate_win(Board, Mark) -> Score = 100000
    ; check_immediate_win(Board, OppMark) -> Score = -100000
    ; evaluate_threats(Board, Mark, ThreatScore),  % Nouveau score de menaces
      evaluate_lines(Board, Mark, MyLines),
      evaluate_lines(Board, OppMark, TheirLines),
      evaluate_center(Board, Mark, CenterVal),
      Score is MyLines - TheirLines + CenterVal + ThreatScore
    ).

% Détecte les colonnes créant deux menaces gagnantes
evaluate_threats(Board, Mark, ThreatScore) :-
    findall(Col, (between(1,7,Col), creates_double_threat(Board, Mark, Col)), Threats),
    length(Threats, N),
    ThreatScore is N * 50.

creates_double_threat(Board, Mark, Col) :-
    valid_move(Board, Col),
    drop_piece(Board, Col, Mark, NewBoard),
    findall(Line, (get_horizontal_line(NewBoard, Line), four_consecutive(Line, Mark)), Wins),
    length(Wins, Count),
    Count >= 2.

% Helper: check if a given Mark is already winning in Board
check_immediate_win(Board, Mark) :-
    player_mark(PID, Mark),
    check_win(Board, PID).

% Evaluate lines: sums partial scores for all 4-length sublines
evaluate_lines(Board, Mark, TotalScore) :-
    findall(Line,
        ( get_horizontal_line(Board, Line)
        ; get_vertical_line(Board, Line)
        ; get_diagonal_line(Board, Line)
        ),
        RawLines),
    % Only consider lines exactly length=4
    include(length4, RawLines, FourLines),
    % Score each line
    findall(V, (member(L, FourLines), score_line(L, Mark, V)), Scores),
    sum_list(Scores, TotalScore).

length4(L) :- length(L, 4).

score_line(Line, Mark, Value) :-
    blank_mark(Blank),
    opponent_mark(Mark, Opp),
    count_pieces(Line, Mark, Mine),
    count_pieces(Line, Blank, Empties),
    count_pieces(Line, Opp, Theirs),
    ( Mine=3, Empties=1 -> Value=100
    ; Mine=2, Empties=2 -> Value=10
    ; Mine=1, Empties=3 -> Value=1
    ; otherwise -> Value=0
    ).

count_pieces(Line, X, Count) :-
    include(=(X), Line, Matches),
    length(Matches, Count).

% Extra center column preference
evaluate_center(Board, Mark, Val) :-
    transpose_board(Board, TBoard),
    nth1(4, TBoard, CenterCol),   % Column 4 is center in a 7-wide board
    include(=(Mark), CenterCol, Hits),
    length(Hits, Count),
    Val is Count * 3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 13. Gathering 4-length lines: horizontal, vertical, diagonal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_horizontal_line(Board, Subline) :-
    member(Row, Board),
    sliding_window(Row, 4, Subline).

get_vertical_line(Board, Subline) :-
    transpose_board(Board, TBoard),
    get_horizontal_line(TBoard, Subline).

get_diagonal_line(Board, Subline) :-
    all_diagonals(Board, AllDiags),
    member(Diag, AllDiags),
    sliding_window(Diag, 4, Subline).

sliding_window(List, Size, Window) :-
    length(Window, Size),
    append(_, Window, List),
    append(Window, _, _).

% Collect all diagonals (down-right + up-right) that have length >= 4
all_diagonals(Board, All) :-
    findall(D1, diag_down_right(Board, D1), DRs),
    findall(D2, diag_up_right(Board, D2), URs),
    append(DRs, URs, All).

% diag_down_right finds all "down-right" diagonals
diag_down_right(Board, Diag) :-
    board_dimensions(Board, MaxR, MaxC),
    between(1, MaxR, Row),
    between(1, MaxC, Col),
    build_down_right(Board, Row, Col, Diag).

build_down_right(Board, R, C, [Cell|Rest]) :-
    get_cell(Board, R, C, Cell),
    R2 is R+1, C2 is C+1,
    build_down_right(Board, R2, C2, Rest).
build_down_right(_, R, C, []) :-
    \+ get_cell(_, R, C, _).

% diag_up_right finds all "up-right" diagonals
diag_up_right(Board, Diag) :-
    board_dimensions(Board, MaxR, MaxC),
    between(1, MaxR, Row),
    between(1, MaxC, Col),
    build_up_right(Board, Row, Col, Diag).

build_up_right(Board, R, C, [Cell|Rest]) :-
    get_cell(Board, R, C, Cell),
    R2 is R-1, C2 is C+1,
    build_up_right(Board, R2, C2, Rest).
build_up_right(_, R, C, []) :-
    \+ get_cell(_, R, C, _).

% board_dimensions: # of rows, # of cols
board_dimensions(Board, Rows, Cols) :-
    length(Board, Rows),
    Board = [FirstRow|_],
    length(FirstRow, Cols).

% get_cell safely
get_cell(Board, Row, Col, Cell) :-
    Row > 0, Col > 0,
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Cell).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 14. transpose_board/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transpose_board([], []).
transpose_board([[]|_], []).
transpose_board([Row|Rows], [Col|Cols]) :-
    transpose_col([Row|Rows], Col, Remainder),
    transpose_board(Remainder, Cols).

transpose_col([], [], []).
transpose_col([[H|T]|Rest], [H|Hs], [T|Ts]) :-
    transpose_col(Rest, Hs, Ts).

<<<<<<< HEAD








% Evaluate the board with a heuristic function
evaluate_board(Board, Score) :-
    player_mark(1, Red),
    player_mark(2, Yellow),
    scoring(Board, Red, RedScore),
    scoring(Board, Yellow, YellowScore),
    (Red = 'R' ->
        Score is RedScore - YellowScore
    ;
        Score is YellowScore - RedScore
    ).

% Simplified scoring function for better performance
scoring(Board, Mark, Score) :-
    check_sequences(Board, Mark, 4, Win),    % Check for wins
    check_sequences(Board, Mark, 3, ThreeSeq),  % Check for three in a row
    check_sequences(Board, Mark, 2, TwoSeq),    % Check for two in a row
    Score is Win * 1000 + ThreeSeq * 100 + TwoSeq * 10.

check_sequences(Board, Mark, Length, Count) :-
    findall(1, (
        (member(Row, Board), consecutive_marks(Row, Mark, Length)) ;
        (transpose_board(Board, Transposed), member(Col, Transposed), consecutive_marks(Col, Mark, Length))
    ), Sequences),
    length(Sequences, Count).

% Horizontal scoring
horizontal_score(Board, Mark, Score) :-
    findall(S, (member(Row, Board), line_score(Row, Mark, S)), Scores),
    sum_list(Scores, Score).

% Vertical scoring
vertical_score(Board, Mark, Score) :-
    transpose_board(Board, Transposed),
    horizontal_score(Transposed, Mark, Score).

% Diagonal scoring
diagonal_score(Board, Mark, Score) :-
    diagonal_win_right_score(Board, Mark, DRScore),
    diagonal_win_left_score(Board, Mark, DLScore),
    Score is DRScore + DLScore.

% Center column scoring
center_score(Board, Mark, Score) :-
    nth1(4, Board, CenterColumn),
    include(==(Mark), CenterColumn, Matches),
    length(Matches, Count),
    Score is Count * 3.

% Helper to score diagonals
diagonal_win_right_score(Board, Mark, Score) :-
    findall(S, (append(_, [Row1, Row2, Row3, Row4|_], Board),
                nth1(N1, Row1, Cell1),
                nth1(N2, Row2, Cell2),
                nth1(N3, Row3, Cell3),
                nth1(N4, Row4, Cell4),
                N2 is N1 + 1, N3 is N2 + 1, N4 is N3 + 1,
                sequence_score([Cell1, Cell2, Cell3, Cell4], Mark, S)),
            Scores),
    sum_list(Scores, Score).

diagonal_win_left_score(Board, Mark, Score) :-
    findall(S, (append(_, [Row1, Row2, Row3, Row4|_], Board),
                nth1(N1, Row1, Cell1),
                nth1(N2, Row2, Cell2),
                nth1(N3, Row3, Cell3),
                nth1(N4, Row4, Cell4),
                N2 is N1 - 1, N3 is N2 - 1, N4 is N3 - 1,
                sequence_score([Cell1, Cell2, Cell3, Cell4], Mark, S)),
            Scores),
    sum_list(Scores, Score).


% Line score helper
line_score(Line, Mark, Score) :-
    findall(S, (sublist_of_length(Line, 4, SubList), 
                sequence_score(SubList, Mark, S)), 
            Scores),
    sum_list(Scores, Score).

sequence_score(Seq, Mark, Score) :-
    include(==(Mark), Seq, Matches),
    length(Matches, MatchCount),
    include(==('_'), Seq, Blanks),
    length(Blanks, BlankCount),
    opponent_mark(_, OpponentMark),
    include(==(OpponentMark), Seq, Opponents),
    length(Opponents, OpponentCount),
    (MatchCount == 4 -> Score is 100000 ;                    % Winning position
     MatchCount == 3, BlankCount == 1 -> Score is 1000 ;     % Three in a row
     MatchCount == 2, BlankCount == 2 -> Score is 100 ;      % Two in a row
     MatchCount == 1, BlankCount == 3 -> Score is 10 ;       % Single piece
     OpponentCount > 0 -> Score is 0 ;                       % Blocked sequence
     Score is 1).     

% Sublist extraction helper
sublist_of_length(List, Length, SubList) :-
    append(_, Rest, List),
    append(SubList, _, Rest),
    length(SubList, Length).

minimax(Board, Depth, Alpha, Beta, true, BestMove, BestScore) :-
    (Depth == 0 ->
        evaluate_board(Board, BestScore),
        BestMove = nil
    ;
        findall(Move, valid_move(Move), Moves),
        (Moves = [] ->
            BestScore = -1000000,
            BestMove = nil
        ;
            alpha_beta_max(Moves, Board, Depth, Alpha, Beta, BestMove, BestScore)
        )
    ).

minimax(Board, Depth, Alpha, Beta, Player, BestMove, BestScore) :-
    (Depth =:= 0 ->
        evaluate_board(Board, BestScore),
        BestMove = -1
    ;
        findall(Move, valid_move(Move), Moves),
        (Moves = [] ->
            evaluate_board(Board, BestScore),
            BestMove = -1
        ;
            player_mark(Player, Mark),
            best_move(Moves, Board, Depth, Alpha, Beta, Player, Mark, -1, BestMove, BestScore)
        )
    ).

alpha_beta_max([], _, _, Alpha, _, nil, Alpha).
alpha_beta_max([Move|Moves], Board, Depth, Alpha, Beta, BestMove, BestScore) :-
    player_mark(1, Mark),
    drop_piece(Board, Move, Mark, NewBoard),
    NewDepth is Depth - 1,
    minimax(NewBoard, NewDepth, Alpha, Beta, false, _, Score),
    (Score > Alpha ->
        NewAlpha = Score,
        CurrentBestMove = Move
    ;
        NewAlpha = Alpha,
        CurrentBestMove = BestMove
    ),
    (NewAlpha >= Beta ->
        BestScore = NewAlpha,
        BestMove = CurrentBestMove
    ;
        alpha_beta_max(Moves, Board, Depth, NewAlpha, Beta, MoveNext, ScoreNext),
        (ScoreNext > NewAlpha ->
            BestScore = ScoreNext,
            BestMove = MoveNext
        ;
            BestScore = NewAlpha,
            BestMove = CurrentBestMove
        )
    ).

alpha_beta_min([], _, _, _, Beta, nil, Beta).
alpha_beta_min([Move|Moves], Board, Depth, Alpha, Beta, BestMove, BestScore) :-
    player_mark(2, Mark),
    drop_piece(Board, Move, Mark, NewBoard),
    NewDepth is Depth - 1,
    minimax(NewBoard, NewDepth, Alpha, Beta, true, _, Score),
    (Score < Beta ->
        NewBeta = Score,
        CurrentBestMove = Move
    ;
        NewBeta = Beta,
        CurrentBestMove = BestMove
    ),
    (Alpha >= NewBeta ->
        BestScore = NewBeta,
        BestMove = CurrentBestMove
    ;
        alpha_beta_min(Moves, Board, Depth, Alpha, NewBeta, MoveNext, ScoreNext),
        (ScoreNext < NewBeta ->
            BestScore = ScoreNext,
            BestMove = MoveNext
        ;
            BestScore = NewBeta,
            BestMove = CurrentBestMove
        )
    ).


make_move(Board, Move, MaximizingPlayer, NewBoard) :-
    player_mark(MaximizingPlayer, Mark),
    drop_piece(Board, Move, Mark, NewBoard).


best_move([], _, _, Alpha, Beta, Player, _, CurrentMove, CurrentMove, Score) :-
    (Player =:= 1 -> Score = Alpha ; Score = Beta).

best_move([Move|Moves], Board, Depth, Alpha, Beta, Player, Mark, CurrentBestMove, BestMove, BestScore) :-
    drop_piece(Board, Move, Mark, NewBoard),
    NewDepth is Depth - 1,
    next_player(Player, NextPlayer),
    minimax(NewBoard, NewDepth, Alpha, Beta, NextPlayer, _, Score),
    
    update_best(Player, Move, Score, Alpha, Beta, CurrentBestMove,
               NewMove, NewAlpha, NewBeta),
    
    (NewAlpha >= Beta ->  % Cutoff check
        BestMove = NewMove,
        BestScore = Score
    ;
        best_move(Moves, Board, Depth, NewAlpha, NewBeta, Player, Mark, 
                 NewMove, BestMove, BestScore)
    ).

update_best(1, Move, Score, Alpha, Beta, CurrentMove, NewMove, NewAlpha, Beta) :-
    (Score > Alpha ->
        NewAlpha = Score,
        NewMove = Move
    ;
        NewAlpha = Alpha,
        NewMove = CurrentMove
    ).

update_best(2, Move, Score, Alpha, Beta, CurrentMove, NewMove, Alpha, NewBeta) :-
    (Score < Beta ->
        NewBeta = Score,
        NewMove = Move
    ;
        NewBeta = Beta,
        NewMove = CurrentMove
    ).

% Update best move found
update_best_move(Move, MoveScore, Alpha, Beta, true, _, Move, MoveScore) :-
    MoveScore > Alpha.
update_best_move(Move, MoveScore, Alpha, Beta, false, _, Move, MoveScore) :-
    MoveScore < Beta.
update_best_move(_, _, _, _, _, CurrentBestMove, CurrentBestMove, BestScore).

cutoff_or_continue([], _, _, _, _, _, BestMove, BestScore).
cutoff_or_continue(_, _, _, Alpha, Beta, true, BestMove, BestScore) :-
    Alpha >= Beta.
cutoff_or_continue(_, _, _, Alpha, Beta, false, BestMove, BestScore) :-
    Alpha >= Beta.
cutoff_or_continue(Moves, Board, Depth, Alpha, Beta, MaximizingPlayer, BestMove, BestScore) :-
    best_move(Moves, Board, Depth, Alpha, Beta, MaximizingPlayer, BestMove, BestMove, BestScore).

% Choose best computer move
choose_computer_move(Mark, Move) :-
    board(Board),
    (Mark = 'R' ->
        Player = 1
    ;
        Player = 2
    ),
    minimax(Board, 4, -1000000, 1000000, Player, Move, _),
    (Move = -1 ->  % If no move was found, choose first valid move
        findall(M, valid_move(M), [Move|_])
    ;
        true
    ).

valid_move(Col) :-
    integer(Col),
    Col >= 1, Col =< 7,
    board(B),
    nth1(1, B, TopRow),
    nth1(Col, TopRow, Cell),
    blank_mark(E),
    Cell = E.

=======
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% End of File
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
>>>>>>> noam
