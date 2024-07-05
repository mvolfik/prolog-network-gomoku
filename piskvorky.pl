:- use_module(library(lists), [ append/3, reverse/2 ]).
:- use_module(library(apply), [ maplist/3 ]).
:- include('lettercodes.pl').

% +BoardState, -Expanded
% add a 1-width "border" of empty tiles around the board
% BoardState is the original board, Expanded will be unified with the expanded board
expand_board_state(BoardState, Expanded) :-
    BoardState = [Row1|_],
    length(Row1, BoardWidth),
    nempty(BoardWidth, EmptyRow),
    append([EmptyRow | BoardState], [EmptyRow], HeightExpanded),
    maplist(expand_row, HeightExpanded, Expanded).

expand_row(Row, Expanded) :- append([e|Row], [e], Expanded).

% unifies the second argument with a list with first/last element removed, respectively
popleft([_|T], T).
popright(L, R) :- append(R, [_], L).

% +BoardState, +ChosenX, +ChosenY, -Trimmed
% after you expand the board, this predicate again removes the border where it is not needed based on the chosen move
% -> e.g. if the chosen coordinate is the top left corner, only bottom and right border is removed
trim_board_state(BoardState, ChosenX, ChosenY, Trimmed) :-
    BoardState = [Row1|_],
    length(Row1, MaxX),
    length(BoardState, MaxY),

    (
        ChosenX = 0
    ->
        B1 = BoardState
    ;
        maplist(popleft, BoardState, B1)
    ),
    (
        ChosenY = 0
    ->
        B2 = B1
    ;
        popleft(B1, B2)
    ),
    (
        ChosenX is MaxX - 1
    ->
        B3 = B2
    ;
        maplist(popright, B2, B3)
    ),
    (
        ChosenY is MaxY - 1
    ->
        Trimmed = B3
    ;
        popright(B3, Trimmed)
    ).

% generates a list of N empty tile markers (`e`)
nempty(0, []) :- !.
nempty(N, [e|T]) :- N1 is N - 1, nempty(N1, T).

% +BoardState, -MaxX, -MaxY
% prints the board with coordinates, so the user can determine their move
% provides output variables with information about the board dimensions
print_board(BoardState, MaxX, MaxY) :-
    BoardState = [Row1|_],
    length(BoardState, RowCount), MaxY is RowCount - 1,
    length(Row1, ColCount), MaxX is ColCount - 1,

    number_lettercodes(MaxX, MaxXCodes), length(MaxXCodes, ColWidth),
    number_codes(MaxY, MaxYCodes), length(MaxYCodes, CoordColWidth),

    print_board_header(MaxX, CoordColWidth, ColWidth),
    print_board_rows(BoardState, 0, CoordColWidth, ColWidth),
    print_board_header(MaxX, CoordColWidth, ColWidth).

print_board_header(MaxX, CoordColWidth, ColWidth) :-
    writensp(CoordColWidth),
    MaxXNoninclusive is MaxX + 1,
    print_board_header_columns(0, MaxXNoninclusive, ColWidth), nl.

print_board_header_columns(X, X, _) :- !.
print_board_header_columns(I, MaxX, ColWidth) :-
    write(" "),
    number_lettercodes(I, ColCodes),
    length(ColCodes, W),
    LeftPad is (ColWidth - W) // 2,
    RightPad is ColWidth - W - LeftPad,
    writensp(LeftPad),
    writec(ColCodes),
    writensp(RightPad),
    I1 is I + 1,
    print_board_header_columns(I1, MaxX, ColWidth).

print_board_rows([], _, _, _) :- !.
print_board_rows([Row|Rows], I, CoordColWidth, ColWidth) :-
    number_codes(I, RowCode),
    length(RowCode, W),
    LeftPad is CoordColWidth - W,
    writensp(LeftPad),
    writec(RowCode),
    ColPadLeft is (ColWidth - 1) // 2,
    ColPadRight is ColWidth - 1 - ColPadLeft,
    print_board_row(Row, ColPadLeft, ColPadRight),
    write(" "),
    writec(RowCode),
    nl,
    I1 is I + 1,
    print_board_rows(Rows, I1, CoordColWidth, ColWidth).

writetile(e) :- write(" ").
writetile(x) :- write("X").
writetile(o) :- write("O").

opposite(x, o).
opposite(o, x).

print_board_row([], _, _) :- !.
print_board_row([Col|Cols], ColPadLeft, ColPadRight) :-
    write(" "),
    writensp(ColPadLeft),
    writetile(Col),
    writensp(ColPadRight),
    print_board_row(Cols, ColPadLeft, ColPadRight).

% +Board, +X, +Y, +NewValue, -OldValue, -NewBoard
% NewBoard is unified with an updated board, where the tile at (X, Y) is set to NewValue
% OldValue is unified with the value that was previously at (X, Y)
set_board_tile([Row | OldTail], X, 0, Value, OldValue, [NewRow | OldTail]) :- !,
    set_row_tile(Row, X, Value, OldValue, NewRow).

set_board_tile([Row | OldTail], X, Y, Value, OldValue, [Row | NewTail]) :-
    Y1 is Y - 1,
    set_board_tile(OldTail, X, Y1, Value, OldValue, NewTail).

set_row_tile([OldValue | OldTail], 0, Value, OldValue, [Value | OldTail]) :- !.
set_row_tile([A | OldTail], X, Value, OldValue, [A | NewTail]) :-
    X1 is X - 1,
    set_row_tile(OldTail, X1, Value, OldValue, NewTail).

% +BoardState, +MaxX, +MaxY, +Mark, -Xout, -Yout, -UpdatedBoard
% Asks the user for a move, validates it (eventually reports the error and asks again),
% and then unifies Xout and Yout with the chosen coordinates and UpdatedBoard with the
% updated board
get_move(BoardState, MaxX, MaxY, Mark, Xout, Yout, UpdatedBoard) :-
    write("Choose your move: "),
    read_line_to_codes(user_input, Line),
    string_codes(Line, LineCodes),
    (
        parse_coord(LineCodes, LetterCodes, Y),
        lettercodes_number(LetterCodes, X),
        X >= 0,
        Y >= 0,
        X =< MaxX,
        Y =< MaxY
    ->
        (
            set_board_tile(BoardState, X, Y, Mark, e, UpdatedBoard)
        ->
            Xout = X,
            Yout = Y
        ;
            write("This tile is already occupied"), nl,
            get_move(BoardState, MaxX, MaxY, Mark, Xout, Yout, UpdatedBoard)
        )
    ;
        write("Please enter valid coordinates like 'B2'"), nl,
        get_move(BoardState, MaxX, MaxY, Mark, Xout, Yout, UpdatedBoard)
    ).

% +LetterCodes, -Number
% checks that a string only consists of digits, and unifies Number with the numeric value
checked_number_codes(Codes, N) :- checked_number_codes_(Codes, Codes, N).

checked_number_codes_([], Full, N) :- number_codes(N, Full).
checked_number_codes_([H|T], Full, N) :- H >= 48, H =< 57, !, checked_number_codes_(T, Full, N).

% +Codes, -LetterCodes, -Number
% Parses a coordinate like 'AB34' into a LetterCodes ('AB') and a Number (34 - a number, not a string)
parse_coord([H|T1], [H|T2], N) :- (H >= 65, H =< 90; H >= 97, H =< 122), !, parse_coord(T1, T2, N).
parse_coord([H|T], [], N) :- H >= 48, H =< 57, !, checked_number_codes([H|T], N).

% writes N spaces to stdout. Used for padding and indenting
writensp(N) :- nspaces_(L, N), string_codes(Str, L), write(Str).
nspaces_([], X) :- X =< 0, !.
nspaces_([32|T], N) :- N1 is N - 1, nspaces_(T, N1).

writec(X) :- string_codes(S, X), write(S).

% unifies with empty lists of any length
empty_lists([]).
empty_lists([ [] | T ]) :- empty_lists(T).

% +Column, +OriginalRows, -NewRows
% given a Column as a list of N items and a table (OriginalRows) of N rows and M columns,
% NewRows is unified with a table of N rows and M+1 columns, where the new first item in each row
% is the corresponding item from the Column
prepend_column([], [], []).
prepend_column(
    [Item | Column],
    [OriginalRow | OriginalRows],
    [[Item | OriginalRow] | NewRows]
    ) :-
        prepend_column(Column, OriginalRows, NewRows).

% +Table, -Transposed
% transposes a table
transpose([], EmptyRows) :- empty_lists(EmptyRows).
transpose([Row | Rows], OutRows) :-
    prepend_column(Row, PreviousOutRows, OutRows),
    transpose(Rows, PreviousOutRows).


% all check_win_... predicates are written as deterministic - theoretically, there could be multiple winners on
% an arbitrary gomoku board, but in our game, one player obviously has to make the winning move first

% fail if there is no winner in this row, else unify Winner with the winning player
check_win_row(Row, Winner) :- check_win_row_(Row, 0, e, Winner).
check_win_row_([], _, _, e) :- !, fail.
check_win_row_([e|T], _, _, Winner) :- !, check_win_row_(T, 0, e, Winner).
check_win_row_([X|_], 4, X, X) :- !.
check_win_row_([X|T], Count, X, Winner) :- !, Count1 is Count + 1, check_win_row_(T, Count1, X, Winner).
check_win_row_([X|T], _, _, Winner) :- check_win_row_(T, 1, X, Winner).

check_win_rows([], _) :- !, fail.
check_win_rows([Row|Tail], Winner) :-
    (
        check_win_row(Row, Winner)
        ; check_win_rows(Tail, Winner)
    ), !.

% same as check_win_row, but for the main diagonal of the matrix
% i.e. always slices the first row and first column
check_win_main_diagonal(Grid, Winner) :- check_win_main_diagonal_(Grid, 0, e, Winner).

check_win_main_diagonal_([], _, _, e) :- !, fail.
check_win_main_diagonal_([ [e|_] | T ], _, _, Winner) :- !,
    maplist(popleft, T, NewT),
    check_win_main_diagonal_(NewT, 0, e, Winner).

check_win_main_diagonal_([ [X|_] | _ ], 4, X, X) :- !.
check_win_main_diagonal_([ [X|_] | T ], Count, X, Winner) :- !,
    Count1 is Count + 1,
    maplist(popleft, T, NewT),
    check_win_main_diagonal_(NewT, Count1, X, Winner).

check_win_main_diagonal_([ [X|_] | T ], _, _, Winner) :-
    maplist(popleft, T, NewT),
    check_win_main_diagonal_(NewT, 1, X, Winner).

% check win on the main diagonal, then repeatedly slice the first row
% and check again (i.e. this checks all diagonals below the main one)
check_win_main_diagonals_down([], _) :- !, fail.
check_win_main_diagonals_down([X|T], W) :-
    (
        check_win_main_diagonal([X|T], W)
        ;
        check_win_main_diagonal(T, W)
    ), !.

% same as check_win_main_diagonals_down, but instead slices the first column,
% therefore checking diagonals above the main one
check_win_main_diagonals_right([[]|_], _) :- !, fail.
check_win_main_diagonals_right(Grid, Winner) :-
    (
        check_win_main_diagonal(Grid, Winner)
    ;
        maplist(popleft, Grid, Right), check_win_main_diagonals_right(Right, Winner)
    ), !.

% check the true main diagonal, then check the lower ones, then the upper ones
check_win_main_diagonals(Grid, Winner) :-
    (
        check_win_main_diagonal(Grid, Winner)
        ;
        popleft(Grid, Bottom), check_win_main_diagonals_down(Bottom, Winner)
        ;
        maplist(popleft, Grid, Right), check_win_main_diagonals_right(Right, Winner)
    ), !.

% +BoardState, -Winner
% unifies Winner with the winning player, if the given gomoku board has one, else fails
check_win(BoardState, Winner) :-
    (
        check_win_rows(BoardState, Winner)
        ;
        check_win_main_diagonals(BoardState, Winner)
        ;
        % transposing the board and checking rows is equivalent to checking columns
        transpose(BoardState, Transposed), check_win_rows(Transposed, Winner)
        ;
        % diagonals in board with reversed rows are diagonals going up-right in the original board
        reverse(BoardState, Reversed), check_win_main_diagonals(Reversed, Winner)
    ), !.
