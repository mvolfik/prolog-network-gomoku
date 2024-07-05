# Network gomoku (five in a row) in Prolog

(yes, you can write network code in prolog)

## How to run

1. Install SWI-Prolog 9.x on a Linux system (Windows _might_ work, but I have no way to test it right now). I tested this specifically on SWI-Prolog 9.3.6 (my machine) and 9.1.2 (Rotunda lab `u-pl0.ms.mff.cuni.cz`).
2. Clone this repository
3. Run `./main.pl -s <port>` to start a "server" (player 1)
4. Clone this repository on another machine (or just open a new terminal on the same PC)
5. Run `./main.pl -c <host> <port>` to connect to the server (player 2). Host can be IP address or a domain name. This creates a connection between the peers and starts the game for both players. The server stops listening for new connections.
6. Play the game. The server plays first, automatically placing an `X` in the center of the board, therefore the first "real" turn is taken by the second client, who plays with `O`.
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

### Networking and game loop

After I figured out how to use the swipl socket library, the network code was relatively straightforward: see the predicates `start_server` and `start_client` in `game_loop.pl`. The high-level game logic of taking turns is also in that file: the logic of both server and client merges in `game_loop`. After the connection is established, the roles of server and client are no longer relevant - the server stops listening for new connections, and just a bidirectional socket remains. In fact, a second server (and eventually a game) can be started on the same port of the same host right after the first game connection is established.

The game _"loop"_ is implemented with recursive calls of `game_loop_inner`, which repeatedly asks the player for a move, sends it to the opponent and waits for a move from them. Server and client start the complementary phases of this loop by calling it with different last argument (`IsMyTurn`). The game loop ends when either player wins or disconnects - then the last call of this predicate succeeds without calling itself again.

The network protocol is simple: a message is 4 bytes and consists of two 16-bit big-endian integers, which specify the X and Y coordinates of the move. Both sides run the same simulation of the game, so even though the coordinates of tiles change as the playing field expands, both get the same result. There is no negotiation of the starting player, the server always plays first (and places an `X` in the center of the board, without any input from the player, therefore the first interactive move is done by the client).

### `piskvorky.pl`

The file `piskvorky.pl` contains (as the name suggests) most of the game-specific logic.

#### `get_move`

`get_move` takes the current board. Then it prompts the player for a move, verifies its validity and eventually asks again if the move was invalid, and returns an updated board together with the move coordinates to be sent to the server.

#### `expand_board_state`, `trim_board_state`

These two predicates take care of the dynamic board expansion. Since a game of gomoku can be potentially infinite (though this implementation is limited by the 2^16 coordinate value limit of the network protocol), the board starts as 1x1, and grows as needed. Before a player is asked to make a move, the board is expanded by 1 row/column in each direction, and then the board is trimmed again, only keeping the added border on the side where the move was placed, if any.

#### `print_board` and friends (`print_board_...`)

Turns out that printing a 2d grid with nicely aligned and padded cells and coordinate headers/footers isn't that simple. These predicates do exactly that.

#### `check_win` and friends (`check_win_...`)

Checking all the eight directions of how winning sequence of tiles can be arranged is also a bit tedious. the `check_win` predicate does that, using the `check_win_...` helper functions which check individual directions. There is a logic for checking a row and for checking a main diagonal of a (non-square) board, and then these predicates are iterated over rows, columns, diagonals below and above the main one, and diagonals in an upside down board (which is equivalent to checking diagonals going from the bottom left to top right).

### Program entry point

The whole project is started by `main.pl`. This is an executable file with a shebang comment that starts the prolog environment with the `main/0` goal. This goal is from the SWI-Prolog `library(main)` module, and it gathers the command-line arguments into a list, and calls the user-defined `main/1` with this list.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
