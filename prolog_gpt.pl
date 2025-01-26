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
    nl, write('Welcome to Connect 4 !'), nl,
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
    write('Board initialized: '), nl.

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
        read(Col),
        make_human_move(Col, Mark)
    ;
        write('Computer is thinking...'), nl,
        sleep(1),
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
    write('Computer '), write(Mark), write(' plays column '), write(Col), nl, !.

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

