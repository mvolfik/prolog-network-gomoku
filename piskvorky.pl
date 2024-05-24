print_board(BoardState, MaxX, MaxY) :-
    write("dummy board print"), nl,
    MaxX = 10,
    MaxY = 5.

get_coords(X, Y, MaxX, MaxY) :-
    read_line_to_string(user_input, Line),
    split_string(Line, " ", "", Input),
    get_coords_parse(X, Y, MaxX, MaxY, Input).


get_coords_parse(X, Y, MaxX, MaxY, [A, B]) :-
    (
        atom_codes(A, ACodes),
        number_from_letters(ACodes, ANum),
        ANum >= 0,
        ANum =< MaxX,
        (
            atom_number(B, BNum),
            BNum >= 0,
            BNum =< MaxY,
            X = ANum,
            Y = BNum,
            write("Got coords: "), write(X), write(", "), write(Y), nl
        ;
            write("The second value must be a number 0 to "), write(MaxY), nl,
            write(": "),
            get_coords(X, Y, MaxX, MaxY)
        )
    ;
        write("The first value must be a letter A to "), write(MaxX), nl,
        write(": "),
        get_coords(X, Y, MaxX, MaxY)
    ); throw(bug).

get_coords_parse(X, Y, MaxX, MaxY, _) :-
    write("Please enter two values separated by a space"), nl,
    write(": "),
    get_coords(X, Y, MaxX, MaxY).


number_from_letters([], Number) :- !, false.
number_from_letters(Letters, Number) :- number_from_letters_(Letters, 0, N), Number is N - 1.

number_from_letters_([], N, N).
number_from_letters_([Letter|Letters], Nin, Nout) :-
    number_from_letter(Letter, Value),
    Nnext is Nin * 26 + Value,
    number_from_letters_(Letters, Nnext, Nout).

number_from_letter(LetterCode, Number) :-
    LetterCode >= 65, LetterCode =< 90, !, Number is LetterCode - 65 + 1
    ;
    LetterCode >= 97, LetterCode =< 122, !, Number is LetterCode - 97 + 1.

letters_from_number(Number, Letters) :- Number >= 0, Nx is Number + 1, letters_from_number_(Nx, [], Letters).

letters_from_number_(0, L, L) :- !.
letters_from_number_(N, Acc, Out) :-
    (
        N =< 26
    ->
        letter_from_number(N, Letter),
        Out = [Letter|Acc]
    ;
        N1 is (N - 1) // 26,
        letter_from_number(N, Letter),
        letters_from_number_(N1, [Letter|Acc], Out)
    ).

letter_from_number(Number, Letter) :-
    Letter is (Number - 1) mod 26 + 65.
