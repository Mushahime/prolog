:- dynamic board/1.
:- dynamic player/2.
:- dynamic ai_strategy/1.   % <--- Pour stocker le choix de stratégie

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
max_depth(4).

% Preferred column ordering for AI (used in find_winning_move/block and minimax):
ordered_columns([4,3,5,2,6,1,7]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2. Main Entry Point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run :-
    choose_ai_strategy,         % <--- NOUVEAU : on demande la stratégie d’IA
    initialize_board,
    nl, write('Welcome to Connect 4!'), nl,
    read_players,
    play(1).   % Start with player 1

% Menu de choix de stratégie, avec 4 options
choose_ai_strategy :-
    nl, write('Choisissez la stratégie de l\'IA :'), nl,
    write('1. IA défensive (seulement les menaces)\n'),
    write('2. Contrôle du centre\n'),
    write('3. Lines only (évaluation par lignes)\n'),
    write('4. Combined (menaces + centre + lignes)\n'),
    write('Votre choix (1,2,3,4) ? '),
    read(Choice),
    ( Choice = 1 ->
        retractall(ai_strategy(_)),
        asserta(ai_strategy(defensive))
    ; Choice = 2 ->
        retractall(ai_strategy(_)),
        asserta(ai_strategy(center_control))
    ; Choice = 3 ->
        retractall(ai_strategy(_)),
        asserta(ai_strategy(lines_only))
    ; Choice = 4 ->
        retractall(ai_strategy(_)),
        asserta(ai_strategy(combined))
    ; write('Choix invalide, réessayez.'), nl,
      choose_ai_strategy
    ).

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

    nl, write('---------------------------------'), nl,
    write('Board initialized.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4. Player Setup: Human or Computer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_players :-
    nl, write('Number of human players (0, 1, or 2)? '),
    read(N),
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
        read(Col),
        make_human_move(Col, Mark)
      ; % Computer
        write('Computer is thinking...'), nl,
        sleep(1),
        choose_computer_move(Mark, BestCol),
        make_computer_move(BestCol, Mark)
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

drop_piece(Board, Col, Mark, NewBoard) :-
    drop_piece_helper(Board, Col, Mark, 6, NewBoard). % Commence à la rangée 6 (bas)

drop_piece_helper(_, _, _, 0, _) :- !, fail. % Échec si colonne pleine

drop_piece_helper(Board, Col, Mark, Row, NewBoard) :-
    nth1(Row, Board, ThisRow),
    nth1(Col, ThisRow, Cell),
    blank_mark(B),
    ( Cell = B -> 
        replace_nth(Board, Row, Col, Mark, NewBoard) % Place en bas
      ; R2 is Row - 1,
        drop_piece_helper(Board, Col, Mark, R2, NewBoard) % Cherche au-dessus
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
    ( 
      check_horizontal_win(Board, Mark)
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

check_diagonal_win(Board, Mark) :-
    board_dimensions(Board, Rows, Cols),
    between(1, Rows, Row),
    between(1, Cols, Col),
    (check_line(Board, Mark, Row, Col, 1, 1)   % Diagonale ↘
    ; check_line(Board, Mark, Row, Col, -1, 1) % Diagonale ↗
    ).

check_line(Board, Mark, Row, Col, DRow, DCol) :-
    get_cell(Board, Row, Col, Mark),
    R2 is Row + DRow, C2 is Col + DCol,
    get_cell(Board, R2, C2, Mark),
    R3 is R2 + DRow, C3 is C2 + DCol,
    get_cell(Board, R3, C3, Mark),
    R4 is R3 + DRow, C4 is C3 + DCol,
    get_cell(Board, R4, C4, Mark).

diagonal_right(Board, Mark) :-
    append(_, [R1,R2,R3,R4|_], Board),
    append(P1,[Mark|_],R1),
    append(P2,[Mark|_],R2),
    append(P3,[Mark|_],R3),
    append(P4,[Mark|_],R4),
    length(P1,N1),
    length(P2,N2), N2 is N1+1,
    length(P3,N3), N3 is N2+1,
    length(P4,N4), N4 is N3+1.

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

four_consecutive(List, Mark) :-
    append(_, [Mark,Mark,Mark,Mark|_], List).

board_full(Board) :-
    blank_mark(B),
    \+ (member(Row, Board), member(B, Row)).

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

% evaluate_columns/4
% Simple tri des colonnes selon un "score potentiel" (Base + Threat)
evaluate_columns(Board, Mark, Cols, OrderedCols) :-
    maplist(column_potential(Board, Mark), Cols, Scores),
    zip(Scores, Cols, Pairs),
    sort(Pairs, Sorted),
    reverse(Sorted, Descending),
    pairs_values(Descending, OrderedCols).

% column_potential/3
% On donne un mini-score de base pour la centralité, plus un bonus
% si la pose donne une victoire immédiate.
column_potential(Board, Mark, Col, Score) :-
    % Bonus central et adjacent
    ( Col =:= 4 -> Base = 20 
    ; (Col =:= 3 ; Col =:= 5) -> Base = 15 
    ; (Col =:= 2 ; Col =:= 6) -> Base = 5
    ; Base = 0 
    ),
    % Menaces immédiates (vict. directe)
    ( valid_move(Board, Col),
      drop_piece(Board, Col, Mark, TempBoard),
      check_immediate_win(TempBoard, Mark) -> Threat = 50
    ; Threat = 0
    ),
    Score is Base + Threat.

% Petit coup de pouce : si le plateau est vide, jouer direct en colonne 4
choose_computer_move(Mark, 4) :-
    board(Board),
    is_empty_board(Board),  % Vérifie si le plateau est vide
    valid_move(Board, 4), !.

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

make_move_temp(Board, Col, Mark, NewBoard) :-
    valid_move(Board, Col),
    drop_piece(Board, Col, Mark, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 12. Position Evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% On dispatch selon la stratégie choisie, parmi :
%   1) defensive
%   2) center_control
%   3) lines_only
%   4) combined
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate_position(Board, Mark, Score) :-
    ai_strategy(Strategy),             % On récupère la stratégie choisie
    evaluate_position_strategy(Strategy, Board, Mark, Score).

% Définit les 4 approches :

% 1) IA défensive (seulement "threats")
evaluate_position_strategy(defensive, Board, Mark, Score) :-
    opponent_mark(Mark, OppMark),
    ( check_immediate_win(Board, Mark) ->
        Score = 100000      % Victoire garantie
    ; check_immediate_win(Board, OppMark) ->
        Score = -100000     % Défaite imminente
    ; 
        evaluate_threats(Board, Mark, ThreatScore),
        Score is ThreatScore
    ).

% 2) Contrôle du centre
evaluate_position_strategy(center_control, Board, Mark, Score) :-
    opponent_mark(Mark, OppMark),
    ( check_immediate_win(Board, Mark) ->
        Score = 100000
    ; check_immediate_win(Board, OppMark) ->
        Score = -100000
    ;
        evaluate_center(Board, Mark, CenterVal),
        % On peut moduler le coefficient (ex. x10)
        Score is CenterVal * 10
    ).

% 3) Lines only (on compte nos lignes et celles de l’adversaire)
evaluate_position_strategy(lines_only, Board, Mark, Score) :-
    opponent_mark(Mark, OppMark),
    ( check_immediate_win(Board, Mark) ->
        Score = 100000
    ; check_immediate_win(Board, OppMark) ->
        Score = -100000
    ;
        evaluate_lines(Board, Mark, MyLines),
        evaluate_lines(Board, OppMark, TheirLines),
        Score is MyLines - TheirLines
    ).

% 4) Combined (menaces + lignes + centre)
evaluate_position_strategy(combined, Board, Mark, Score) :-
    opponent_mark(Mark, OppMark),
    ( check_immediate_win(Board, Mark) -> 
        Score = 100000  
    ; check_immediate_win(Board, OppMark) -> 
        Score = -100000 
    ; 
        evaluate_threats(Board, Mark, ThreatScore),
        evaluate_lines(Board, Mark, MyLines),
        evaluate_lines(Board, OppMark, TheirLines),
        evaluate_center(Board, Mark, CenterVal),
        
        % Pondération des composantes
        % Ex : on additionne tout avec quelques coefficients de pondération
        WeightedThreat = ThreatScore,
        WeightedLines  = MyLines - TheirLines,
        WeightedCenter = CenterVal * 3, 
        
        Score0 is WeightedThreat + WeightedLines + WeightedCenter,

        % Clip final pour éviter des scores trop énormes
        ( Score0 > 10000  -> Score = 10000
        ; Score0 < -10000 -> Score = -10000
        ; Score = Score0
        )
    ).

% Vérifie si un Mark a un alignement gagnant "tout de suite"
check_immediate_win(Board, Mark) :-
    player_mark(PID, Mark),
    check_win(Board, PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 13. Évaluation des menaces + lignes + centre
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---- Menaces (Threats) ----

evaluate_threats(Board, Mark, Score) :-
    findall(
        Threat,
        ( between(1,7,Col),
          valid_move(Board, Col),
          drop_piece(Board, Col, Mark, NewBoard),
          threat_value(NewBoard, Mark, Threat)
        ),
        ThreatList
    ),
    sum_list(ThreatList, Score).

% Simple : si la pose dans Col crée une situation avec potentiellement
% 3 pions alignés + 1 trou => menace
threat_value(NewBoard, Mark, Value) :-
    ( has_potential_win(NewBoard, Mark) -> Value = 50
    ; Value = 0
    ).

has_potential_win(Board, Mark) :-
    ( get_horizontal_line(Board, Line)
    ; get_vertical_line(Board, Line)
    ; get_diagonal_line(Board, Line)
    ),
    potential_three(Line, Mark).

potential_three(Line, Mark) :-
    count_pieces(Line, Mark, 3),
    count_pieces(Line, '_', 1).

% ---- Évaluation des lignes (utilisée par lines_only & combined) ----

evaluate_lines(Board, Mark, TotalScore) :-
    findall(Line,
        ( get_horizontal_line(Board, Line)
        ; get_vertical_line(Board, Line)
        ; get_diagonal_line(Board, Line)
        ),
        RawLines),
    % Ne prendre que les sous-lignes de longueur 4
    include(length4, RawLines, FourLines),
    findall(V, (member(L, FourLines), score_line(L, Mark, V)), Scores),
    sum_list(Scores, TotalScore).

length4(L) :- length(L, 4).

score_line(Line, Mark, Value) :-
    blank_mark(Blank),
    opponent_mark(Mark, Opp),
    
    count_pieces(Line, Mark, Mine),
    count_pieces(Line, Opp, Theirs),
    count_pieces(Line, Blank, Blanks),
    
    offensive_score(Mine, Blanks, Offensive),
    defensive_score(Theirs, Blanks, Defensive),
    
    % Pondération par défaut (2 pour l'offensive, 1.5 pour la défensive)
    Value is (Offensive * 2) - (Defensive * 1.5).

offensive_score(Mine, Blanks, Score) :-
    ( Mine == 4 -> Score = 100000   % Alignement déjà complet
    ; Mine == 3, Blanks == 1 -> Score = 200
    ; Mine == 2, Blanks == 2 -> Score = 20
    ; Mine == 1, Blanks == 3 -> Score = 2
    ; Score = 0
    ).

defensive_score(Theirs, Blanks, Score) :-
    ( Theirs == 4 -> Score = 100000
    ; Theirs == 3, Blanks == 1 -> Score = 1000
    ; Theirs == 2, Blanks == 2 -> Score = 30
    ; Theirs == 1, Blanks == 3 -> Score = 3
    ; Score = 0
    ).

count_pieces(Line, Piece, Count) :-
    include(==(Piece), Line, Matches),
    length(Matches, Count).

% ---- Évaluation du centre ----

evaluate_center(Board, Mark, Val) :-
    transpose_board(Board, TBoard),
    nth1(4, TBoard, CenterCol),  
    include(=(Mark), CenterCol, Hits),
    length(Hits, Count),
    Val is Count * 1.5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 14. Récupération des lignes horizontales, verticales, diagonales
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

all_diagonals(Board, All) :-
    findall(D1, diag_down_right(Board, D1), DRs),
    findall(D2, diag_up_right(Board, D2), URs),
    append(DRs, URs, All).

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

board_dimensions(Board, Rows, Cols) :-
    length(Board, Rows),
    Board = [FirstRow|_],
    length(FirstRow, Cols).

get_cell(Board, Row, Col, Cell) :-
    Row > 0, Col > 0,
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Cell).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 15. transpose_board/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transpose_board([], []).
transpose_board([[]|_], []).
transpose_board([Row|Rows], [Col|Cols]) :-
    transpose_col([Row|Rows], Col, Remainder),
    transpose_board(Remainder, Cols).

transpose_col([], [], []).
transpose_col([[H|T]|Rest], [H|Hs], [T|Ts]) :-
    transpose_col(Rest, Hs, Ts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% End of File
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
