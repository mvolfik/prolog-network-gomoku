#!/usr/bin/env -S sh -c "exec swipl -q -g main -t halt \$0 -- \"\$@\""

:- set_prolog_flag(autoload, false).
:- use_module(library(main)).
:- include('piskvorky.pl').

main(_) :- verify_letters_number_convertor(80000).

verify_letters_number_convertor(0) :- !.
verify_letters_number_convertor(N) :-
    letters_from_number(N, L),
    number_from_letters(L, Nx),
    (
        Nx = N, !
        ;
        write("N: "), write(N), nl,
        write("L: "), write(L), nl,
        write("Nx: "), write(Nx), nl
    ),
    N1 is N - 1,
    verify_letters_number_convertor(N1).
