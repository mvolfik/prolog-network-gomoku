#!/usr/bin/env -S swipl -q -t main

print_args([]).
print_args([Arg | T]) :-
    write(Arg), nl,
    print_args(T).

main(Args) :- print_args(Args).
