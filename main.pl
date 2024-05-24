#!/usr/bin/env -S swipl -q -g main -t halt

print_args([]).
print_args([Arg | T]) :-
    write(Arg), nl,
    print_args(T).

main(Args) :- print_args(Args).
