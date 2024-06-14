#!/usr/bin/env -S sh -c "exec swipl -g main -t halt \$0 -- \"\$@\""

:- set_prolog_flag(autoload, explicit).
:- set_prolog_flag(verbose_autoload, true).
:- set_prolog_flag(warn_autoload, true).

:- use_module(library(main)).
:- use_module(library(edinburgh)).
:- use_module(library(prolog_trace)).
:- include('piskvorky.pl').

main(_) :- debug,
    % lettercodes_number(`a`, 0), lettercodes_number(`z`, 25), lettercodes_number(`aa`, 26),
    % lettercodes_number(`zz`, X), lettercodes_number(`aaa`, Y), Y is X + 1,
    % verify_codes_number_convertor(80000),
    R = [x, o, x, x, x, e,e,e,e,o,o],
    print_board([R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R,R], MaxX, MaxY),
    write("MaxX: "), write(MaxX), nl,
    write("MaxY: "), write(MaxY), nl,
    trim_board_state([[1,2,3],[4,5,6],[7,8,9]], 0, 0, [[1,2],[4,5]]).

verify_codes_number_convertor(0) :- !.
verify_codes_number_convertor(N) :-
    number_lettercodes(N, L),
    lettercodes_number(L, Nx),
    (
        Nx = N, !
        ;
        write("N: "), write(N), nl,
        write("L: "), write(L), nl,
        write("Nx: "), write(Nx), nl
    ),
    N1 is N - 1,
    verify_codes_number_convertor(N1).
