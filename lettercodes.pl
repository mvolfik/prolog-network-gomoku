:- use_module(library(arithmetic), [ arithmetic_function/1 ]).

% define alphabet_size as an arithmetic "constant"
alphabet_size(X) :- X is 3.
:- arithmetic_function(alphabet_size/0).

lettercodes_number(``, _) :- !, false.
lettercodes_number(Codes, Number) :- lettercodes_number_(Codes, 0, N), Number is N - 1.

lettercodes_number_([], N, N).
lettercodes_number_([Code|Codes], Nin, Nout) :-
    lettercode_number(Code, Value),
    Nnext is Nin * alphabet_size + Value,
    lettercodes_number_(Codes, Nnext, Nout).

lettercode_number(Code, Number) :-
    Code >= 65, Code =< 90, !, Number is Code - 65 + 1
    ;
    Code >= 97, Code =< 122, !, Number is Code - 97 + 1.

number_lettercodes(Number, Codes) :- Number >= 0, Nx is Number + 1, number_lettercodes_(Nx, [], Codes).

number_lettercodes_(0, L, L) :- !.
number_lettercodes_(N, Acc, Out) :-
    (
        N =< alphabet_size
    ->
        number_lettercode(N, Code),
        Out = [Code|Acc]
    ;
        N1 is (N - 1) // alphabet_size,
        number_lettercode(N, Code),
        number_lettercodes_(N1, [Code|Acc], Out)
    ).

number_lettercode(Number, Code) :-
    Code is (Number - 1) mod alphabet_size + 65.
