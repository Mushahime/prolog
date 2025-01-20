% game_over(+Board, -Winner)
% Succeeds if there's a winner or it's a draw.
% Winner can be 'r' for Red, 'y' for Yellow, or 'draw'.
game_over(Board, Winner) :-
    ( winner(Board, r) -> Winner = r
    ; winner(Board, y) -> Winner = y
    ; board_full(Board) -> Winner = draw
    ), !.

% board_full(+Board)
% True if no empty cells remain in top row
board_full(Board) :-
    \+ valid_moves(Board, _).  % If no valid moves, board is full.

% winner(+Board, +Player)
% True if Player has 4 in a row somewhere on the board.
winner(Board, Player) :-
    % Typically you'd check all 4-in-a-row patterns:
    ( horizontal_win(Board, Player)
    ; vertical_win(Board, Player)
    ; diagonal_win(Board, Player)
    ).

% (Implementation details for checking horizontal_win/2, vertical_win/2, 
%  and diagonal_win/2 are omitted here for brevity but typically involve
%  scanning for sequences of four identical pieces.)





% evaluate_board(+Board, +Player, -Score)
% Returns a heuristic Score from Player's perspective.
evaluate_board(Board, Player, Score) :-
    ( game_over(Board, Winner) ->
        terminal_score(Winner, Player, Score)
    ; basic_heuristic(Board, Player, Score)
    ).

% terminal_score(+Winner, +Player, -Score)
% If Winner = Player => large positive, if draw => 0, else large negative
terminal_score(draw, _, 0) :- !.
terminal_score(Winner, Player, 10000) :- Winner = Player, !.
terminal_score(_, _, -10000).  % Lost

% basic_heuristic(+Board, +Player, -Score)
basic_heuristic(Board, Player, Score) :-
    center_score(Board, Player, CenterVal),
    % You could add more partial scores here, e.g., 
    % two_in_a_rows(Board, Player, TwoVal), 
    % three_in_a_rows(Board, Player, ThreeVal),
    Score is CenterVal.

% center_score(+Board, +Player, -CenterVal)
% Count how many pieces the Player has in the center column (col = 3).
% Subtract how many pieces the opponent has in col = 3.
center_score(Board, Player, CenterVal) :-
    opponent(Player, Opp),
    count_in_column(Board, 3, Player, PlayerCount),
    count_in_column(Board, 3, Opp, OppCount),
    % Weight center advantage; e.g. each piece is worth 4 points
    CenterVal is (PlayerCount - OppCount) * 4.

% count_in_column(+Board, +Col, +Piece, -Count)
count_in_column(Board, Col, Piece, Count) :-
    findall(_, (
        nth0(Row, Board, RowList),
        nth0(Col, RowList, Cell),
        Cell = Piece
    ), Bag),
    length(Bag, Count).

% opponent(+Player, -Opp)
% Switch between 'r' and 'y', or however you designate your players.
opponent(r, y).
opponent(y, r).






/*************************************
 * choose_move/4
 *************************************/
% choose_move(+Board, +Depth, +Player, -BestMove)
%
% Entry point for selecting the best move:
% - Board: current board state
% - Depth: how many levels deep to search
% - Player: whose turn it is (e.g., 'r' or 'y')
% - BestMove: returned column index that is best for 'Player'
%
% This predicate calls alpha_beta/7 to perform the alpha-beta Minimax search,
% and ignores the final score (returned in _Score), returning only the best move.
choose_move(Board, Depth, Player, BestMove) :-
    alpha_beta(Board, Depth, -10000, 10000, Player, BestMove, _Score).



/*************************************
 * alpha_beta/7
 *************************************/
% alpha_beta(+Board, +Depth, +Alpha, +Beta, +Player, -BestMove, -BestScore)
%
% This is the "Max" branch of alpha-beta search:
% - Board: current board configuration
% - Depth: current depth of the search (will be decreased each recursive call)
% - Alpha: the best (highest) score found so far along the path to the root
% - Beta: the best (lowest) score found so far for the minimizing player
% - Player: the player for which we are maximizing the score
% - BestMove: the best move found at this level of search
% - BestScore: the score associated with BestMove
%
% Operation:
% 1) If we've reached depth 0 or the position is terminal (win/loss/draw),
%    we simply evaluate the board and return that evaluation (no moves).
% 2) Otherwise, generate all valid moves and explore them in best_move_loop/8.
%    This loop tries each move, calls alpha_beta_min/7 (the "Min" side),
%    and picks the move leading to the best (maximum) score.
%    We update 'Alpha' with the best score found and prune when Alpha >= Beta.

alpha_beta(Board, Depth, Alpha, Beta, Player, BestMove, BestScore) :-
    % Base Case: if depth is 0 or the game is over, evaluate position.
    ( Depth =:= 0 ; game_over(Board, _Winner) ) ->
        evaluate_board(Board, Player, Eval),
        BestMove = none,
        BestScore = Eval
    ;
        % Otherwise, find all valid moves (all columns not full).
        valid_moves(Board, Moves),
        ( Moves = [] ->
            % No valid moves => board is effectively full => evaluate position.
            evaluate_board(Board, Player, Eval),
            BestMove = none,
            BestScore = Eval
        ;
            % Recurse: we go one level deeper for each candidate move.
            NewDepth is Depth - 1,
            % best_move_loop tries each move in Moves, calling alpha_beta_min/7,
            % and picks the move that yields the highest score.
            best_move_loop(Moves, Board, Player, NewDepth, Alpha, Beta,
                           nil-(-99999),    % This holds the 'current best' so far.
                           BestMove-BestScore)
        )
    ).



/*************************************
 * best_move_loop/8
 *************************************/
% best_move_loop(+Moves, +Board, +Player, +Depth, +Alpha, +Beta, +CurrentBest, -Best)
%
% This predicate iterates over the list of possible moves (Moves) to find
% the "best" move for the maximizing player:
%
% - Moves: list of valid column indices
% - Board: current board
% - Player: current maximizing player
% - Depth: search depth remaining
% - Alpha: current alpha value (best value so far for maximizing player)
% - Beta: current beta value (best value so far for minimizing player)
% - CurrentBest: the best move-score pair found so far (e.g., Move-Score)
% - Best: will unify with the final best move-score pair after considering all moves
%
% Flow:
% - If Moves list is empty, unify Best with CurrentBest (no changes).
% - Otherwise, for each Move:
%   1) Simulate the move (make_move).
%   2) Call alpha_beta_min to evaluate from the opponent's perspective.
%   3) Compare the returned score to the CurrentBest, updating if better.
%   4) Update Alpha = max(Alpha, Score).
%   5) If Alpha >= Beta, prune (stop searching further moves).
%   6) Otherwise, keep iterating over the remaining Moves.
best_move_loop([], _Board, _Player, _Depth, _Alpha, _Beta, Best, Best).
best_move_loop([Move|Rest], Board, Player, Depth, Alpha, Beta, CurrentBest, Best) :-
    % Simulate dropping a piece in column Move for Player -> TempBoard
    make_move(Board, Move, Player, TempBoard),

    % Switch to the opponent, who is now the minimizing player
    opponent(Player, Opp),

    % Evaluate the move from the minimizing side
    alpha_beta_min(TempBoard, Depth, Alpha, Beta, Opp, _ReturnedMove, Score),

    % Update the best move so far (maximize).
    update_best(Move-Score, CurrentBest, NewBest),

    % Update Alpha
    NewAlpha is max(Alpha, Score),

    % Pruning condition: If alpha >= beta, no need to consider further moves
    ( NewAlpha >= Beta ->
        Best = NewBest
    ;
        % Otherwise, continue exploring the remaining moves
        best_move_loop(Rest, Board, Player, Depth, NewAlpha, Beta, NewBest, Best)
    ).



/*************************************
 * alpha_beta_min/7
 *************************************/
% alpha_beta_min(+Board, +Depth, +Alpha, +Beta, +Player, -BestMove, -BestScore)
%
% This is the "Min" branch of alpha-beta search:
% - Board: current board configuration
% - Depth: current depth of the search
% - Alpha: the best (highest) score so far for the maximizing player
% - Beta: the best (lowest) score so far for the minimizing player
% - Player: the player for which we are *minimizing* the score
% - BestMove: best move for the minimizing player at this level
% - BestScore: the score associated with that move
%
% Operation:
% 1) If we've reached Depth 0 or the position is terminal, evaluate the board.
% 2) Otherwise, we get valid moves for the minimizing player and call
%    min_move_loop/8 to pick the move that yields the lowest score.
%    We update Beta with the best (lowest) score and prune when Alpha >= Beta.
alpha_beta_min(Board, Depth, Alpha, Beta, Player, BestMove, BestScore) :-
    % Base Case: if no more depth or game is over, evaluate position.
    ( Depth =:= 0 ; game_over(Board, _Winner) ) ->
        evaluate_board(Board, Player, Eval),
        BestMove = none,
        BestScore = Eval
    ;
        % Otherwise generate all valid moves for the minimizer
        valid_moves(Board, Moves),
        ( Moves = [] ->
            % No valid moves => board is effectively full => evaluate
            evaluate_board(Board, Player, Eval),
            BestMove = none,
            BestScore = Eval
        ;
            % We explore each move, but from a minimizing viewpoint
            NewDepth is Depth - 1,
            min_move_loop(Moves, Board, Player, NewDepth, Alpha, Beta,
                          nil-99999,        % This is our 'current worst' so far
                          BestMove-BestScore)
        )
    ).



/*************************************
 * min_move_loop/8
 *************************************/
% min_move_loop(+Moves, +Board, +Player, +Depth, +Alpha, +Beta, +CurrentWorst, -Best)
%
% This predicate iterates over possible moves for the *minimizing* player.
%
% - Moves: list of valid moves (column indices)
% - Board: current board
% - Player: current minimizing player
% - Depth: remaining depth
% - Alpha: alpha value from the maximizing player's perspective
% - Beta: beta value from the minimizing player's perspective
% - CurrentWorst: the best so far for the minimizing player (lowest score)
% - Best: unifies with the final chosen move-score pair
%
% Flow:
% - If no moves remain, unify Best with CurrentWorst.
% - Otherwise:
%   1) Simulate the move -> TempBoard
%   2) Switch to the maximizing player (Opp)
%   3) Evaluate the move by calling alpha_beta/7 (max side)
%   4) update_worst/3 picks the lower score if better for the minimizer
%   5) Beta is updated => Beta = min(Beta, Score)
%   6) Prune if Alpha >= Beta
min_move_loop([], _Board, _Player, _Depth, _Alpha, _Beta, Best, Best).
min_move_loop([Move|Rest], Board, Player, Depth, Alpha, Beta, CurrentWorst, Best) :-
    % Simulate the move for the minimizing player
    make_move(Board, Move, Player, TempBoard),

    % Switch to the maximizing player
    opponent(Player, Opp),

    % Evaluate the resulting position from the max perspective
    alpha_beta(TempBoard, Depth, Alpha, Beta, Opp, _ReturnedMove, Score),

    % The minimizing player wants to *lower* the score
    update_worst(Move-Score, CurrentWorst, NewBest),

    % Update Beta
    NewBeta is min(Beta, Score),

    % Pruning condition: if Alpha >= Beta => no further exploration
    ( Alpha >= NewBeta ->
        Best = NewBest
    ;
        min_move_loop(Rest, Board, Player, Depth, Alpha, NewBeta, NewBest, Best)
    ).



/*************************************
 * update_best/3
 *************************************/
% update_best(+Candidate, +CurrentBest, -NewBest)
%
% For a maximizing node:
% - Candidate: a Move-Score pair we just calculated
% - CurrentBest: the Move-Score pair considered "best" so far
% - NewBest: whichever of Candidate or CurrentBest has the higher Score
%
% Note: nil-(-99999) is used initially to indicate we have no best-yet candidate.
update_best(Move-Score, nil-(-99999), Move-Score) :- !.  % If this is the first candidate
update_best(Move-Score, _Move2-Score2, Move-Score) :- Score > Score2, !.
update_best(_Move-Score, Move2-Score2, Move2-Score2).  % Otherwise keep the old best



/*************************************
 * update_worst/3
 *************************************/
% update_worst(+Candidate, +CurrentWorst, -NewWorst)
%
% For a minimizing node:
% - Candidate: a Move-Score pair we just calculated
% - CurrentWorst: the Move-Score pair considered "worst" (lowest) so far
% - NewWorst: whichever has the lower Score
%
% Note: nil-99999 is used initially to indicate no candidate so far for the minimizer.
update_worst(Move-Score, nil-99999, Move-Score) :- !.  % If first candidate
update_worst(Move-Score, _Move2-Score2, Move-Score) :- Score < Score2, !.
update_worst(_Move-Score, Move2-Score2, Move2-Score2).

% when it s the AI turn
board(Board),
Depth = 5,            % or whichever search depth you want
Player = r,           % whichever side the AI is
choose_move(Board, Depth, Player, BestMove),
make_move(Board, BestMove, Player, NewBoard),
retractall(board(_)),
asserta(board(NewBoard)),
display_board(NewBoard).
