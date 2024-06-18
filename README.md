# Network gomoku (five in a row) in Prolog

(yes, you can write network code in prolog)

## How to run

1. Install SWI-Prolog 9.x on a Linux system (Windows _might_ work, but I have no way to test it right now). I tested this specifically on SWI-Prolog 9.3.6 (my machine) and 9.1.2 (Rotunda lab `u-pl0.ms.mff.cuni.cz`).
2. Clone this repository
3. Run `./main.pl -s <port>` to start a "server" (player 1)
4. Clone this repository on another machine (or just open a new terminal on the same PC)
5. Run `./main.pl -c <host> <port>` to connect to the server (player 2). This creates a connection between the peers and starts the game for both players. The server stops listening for new connections.
6. Play the game.
7. The program exits when either player wins or disconnects.

When it's your turn, the game will always prompt you like this:

```
  A B C D E
0           0
1   O O X   1
2     X O   2
3   X       3
4           4
  A B C D E
Choose your move: 
```

Respond with the coordinates of your move, e.g. `A1` to place your stone to the left of your leftmost stone in the example above. The playing field grows in all directions as needed - there is always at least one empty row and column on each side of the board.

## How it works

After I figured out how to use the swipl socket library, the network code was relatively straightforward: see the predicates `start_server` and `start_client` in `game_loop.pl`. The high-level game logic of taking turns is also in that file. In `piskvorky.pl`, there are groups of predicates for three main purposes: detecting if either player has won, printing the game board and getting the user input (choosing the next move). Finally, `main.pl` is an executable file with a shebang comment that starts the prolog environment with the `main` goal, which receives the command-line arguments and starts server or client accordingly.

Additionally, there is `test.pl` which contains small unit tests for some utility predicates (especially converting between numbers and the `A-Z,AA-AZ,BA-BZ,...` coordinate system).

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
