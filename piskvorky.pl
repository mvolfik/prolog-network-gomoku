:- use_module(library(lists), [ append/3, reverse/2 ]).
:- use_module(library(apply), [ maplist/3 ]).
:- include('lettercodes.pl').

expand_board_state(BoardState, Expanded) :-
    BoardState = [Row1|_],
    length(Row1, BoardWidth),
    nempty(BoardWidth, EmptyRow),
    append([EmptyRow | BoardState], [EmptyRow], HeightExpanded),
    maplist(expand_row, HeightExpanded, Expanded).

expand_row(Row, Expanded) :- append([e|Row], [e], Expanded).

popleft([_|T], T).
popright(L, R) :- append(R, [_], L).

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


nempty(0, []) :- !.
nempty(N, [e|T]) :- N1 is N - 1, nempty(N1, T).

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
    print_board_columns(Row, ColPadLeft, ColPadRight),
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

print_board_columns([], _, _) :- !.
print_board_columns([Col|Cols], ColPadLeft, ColPadRight) :-
    write(" "),
    writensp(ColPadLeft),
    writetile(Col),
    writensp(ColPadRight),
    print_board_columns(Cols, ColPadLeft, ColPadRight).

% +Board, +X, +Y, +NewValue, -OldValue, -NewBoard
set_board_tile([Row | OldTail], X, 0, Value, OldValue, [NewRow | OldTail]) :- !,
    set_row_tile(Row, X, Value, OldValue, NewRow).

set_board_tile([Row | OldTail], X, Y, Value, OldValue, [Row | NewTail]) :-
    Y1 is Y - 1,
    set_board_tile(OldTail, X, Y1, Value, OldValue, NewTail).

set_row_tile([OldValue | OldTail], 0, Value, OldValue, [Value | OldTail]) :- !.
set_row_tile([A | OldTail], X, Value, OldValue, [A | NewTail]) :-
    X1 is X - 1,
    set_row_tile(OldTail, X1, Value, OldValue, NewTail).

get_move(ExpandedBoardState, MaxX, MaxY, Mark, Xout, Yout, UpdatedBoard) :-
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
            set_board_tile(ExpandedBoardState, X, Y, Mark, e, UpdatedBoard)
        ->
            Xout = X,
            Yout = Y
        ;
            write("This tile is already occupied"), nl,
            get_move(ExpandedBoardState, MaxX, MaxY, Mark, Xout, Yout, UpdatedBoard)
        )
    ;
        write("Please enter valid coordinates like 'B2'"), nl,
        get_move(ExpandedBoardState, MaxX, MaxY, Mark, Xout, Yout, UpdatedBoard)
    ).

checked_number_codes([], Full, N) :- number_codes(N, Full).
checked_number_codes([H|T], Full, N) :- H >= 48, H =< 57, !, checked_number_codes(T, Full, N).

% +codes, -lettercodes, -number
parse_coord([H|T1], [H|T2], N) :- (H >= 65, H =< 90; H >= 97, H =< 122), !, parse_coord(T1, T2, N).
parse_coord([H|T], [], N) :- H >= 48, H =< 57, !, checked_number_codes([H|T], [H|T], N).

writensp(N) :- nspaces_(L, N), string_codes(Str, L), write(Str).
nspaces_([], X) :- X =< 0, !.
nspaces_([32|T], N) :- N1 is N - 1, nspaces_(T, N1).

writec(X) :- string_codes(S, X), write(S).

% this transpose impl is copied from some old hw, i hope it works
empty_lists([]).
empty_lists([ [] | T ]) :- empty_lists(T).

prepend_column([], [], []).
prepend_column(
    [Item | Column],
    [OriginalRow | OriginalRows],
    [[Item | OriginalRow] | NewRows]
    ) :-
        prepend_column(Column, OriginalRows, NewRows).

transpose([], EmptyRows) :- empty_lists(EmptyRows).
transpose([Row | Rows], OutRows) :-
    prepend_column(Row, PreviousOutRows, OutRows),
    transpose(Rows, PreviousOutRows).


% all check_win_... predicates are written as deterministic - theoretically, there could be multiple winners on
% an arbitrary gomoku board, but in our game, one player obviously has to make the winning move first

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
% i.e. not slicing individual elements from start of list, but always the first row and first column
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

check_win_main_diagonals_down([], _) :- !, fail.
check_win_main_diagonals_down([X|T], W) :-
    (
        check_win_main_diagonal([X|T], W)
        ;
        check_win_main_diagonal(T, W)
    ), !.

check_win_main_diagonals_right([[]|_], _) :- !, fail.
check_win_main_diagonals_right(Grid, Winner) :-
    (
        check_win_main_diagonal(Grid, Winner)
    ;
        maplist(popleft, Grid, Right), check_win_main_diagonals_right(Right, Winner)
    ), !.

% check the true main diagonal, else check the lower ones, then the upper ones
check_win_main_diagonals(Grid, Winner) :-
    (
        check_win_main_diagonal(Grid, Winner)
        ;
        popleft(Grid, Bottom), check_win_main_diagonals_down(Bottom, Winner)
        ;
        maplist(popleft, Grid, Right), check_win_main_diagonals_right(Right, Winner)
    ), !.

check_win(BoardState, Winner) :-
    (
        check_win_rows(BoardState, Winner)
        ;
        check_win_main_diagonals(BoardState, Winner)
        ;
        transpose(BoardState, Transposed), check_win_rows(Transposed, Winner)
        ;
        reverse(BoardState, Reversed), check_win_main_diagonals(Reversed, Winner)
    ), !.
