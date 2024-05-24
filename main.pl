#!/usr/bin/env -S sh -c "exec swipl -q -g main -t halt \$0 -- \$@"

:- set_prolog_flag(autoload, false).
:- use_module(library(main)).
:- include('piskvorky.pl').

print_usage :-
    write("Usage:"), nl,
    write("  To start a server (player 1): ./main.pl -s <port>"), nl,
    write("  To start a client (player 2): ./main.pl -c <host> <port>"), nl.

process_args([]) :-
    write("No arguments provided."), nl,
    print_usage,
    halt(1).

process_args(['-s', Port]) :- start_server(Port).
process_args(['-c', Host, Port]) :- start_client(Host, Port).

process_args(Args) :-
    write("Invalid arguments provided:"), nl,
    print_args(Args),
    nl,
    print_usage,
    halt(1).

print_args([]).
print_args([Arg|Args]) :-
    write("Arg: "), write(Arg), nl,
    print_args(Args).

main(Args) :- process_args(Args).
