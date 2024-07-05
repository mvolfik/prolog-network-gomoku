#!/usr/bin/env -S sh -c "exec swipl -g main -t halt \$0 -- \"\$@\""

:- set_prolog_flag(autoload, explicit).

:- use_module(library(main)).
:- include('game_loop.pl').

print_usage :-
    write("Usage:"), nl,
    write("  To start a server (player 1): ./main.pl -s <port>"), nl,
    write("  To start a client (player 2): ./main.pl -c <host> <port>"), nl.

process_args([]) :-
    write("No arguments provided."), nl,
    print_usage,
    halt(1).

process_args(['-s', Port]) :- !,
    (
        atom_number(Port, PortNum)
        ;
        write("Failed to parse number: "), write(Port), nl,
        print_usage,
        halt(1)
    ), !,
    start_server(PortNum).

process_args(['-c', Host, Port]) :- !,
    (
        atom_number(Port, PortNum)
        ;
        write("Failed to parse number: "), write(Port), nl,
        print_usage,
        halt(1)
    ), !,
    start_client(Host, PortNum).

% if neither the server nor the client command structure matches
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

% setup a signal handler, so that the program nicely exists on Ctrl+C
setup_sigint_handler :-
    % the signal handler doesn't work on Prolog 8.4.2
    on_signal(int, _, throw).

% the goal main/1 is called by main/0 (which we start in shebang at the top)
% with the command line arguments used for starting the script
main(Args) :- setup_sigint_handler, process_args(Args).
