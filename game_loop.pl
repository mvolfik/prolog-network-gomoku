:- use_module(library(socket)).
:- use_module(library(readutil)).
:- include('piskvorky.pl').

% shared cleanup after server and client
% since we don't open any files etc, we just need to close the socket to properly end the connection
cleanup(StreamPair) :-
    % write("Cleaning up"), nl,
    close(StreamPair).

% starts the server for Player 1, listening on the given port on all interfaces
start_server(Port) :-
    write("Waiting for other player to connect on port "), write(Port), nl,

    % prepare the socket
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    tcp_open_socket(Socket, AcceptFd),

    % wait for a connection
    tcp_accept(AcceptFd, GameSocket, PeerIP),
    write("Accepted connection from "), write(PeerIP), nl,

    % close the server socket, do not listen for more connections
    close(AcceptFd),
    setup_call_cleanup(
        (
            tcp_open_socket(GameSocket, StreamPair)
        ),
        game_loop(StreamPair, x),
        cleanup(StreamPair)
    ).

% connect to the given server (Host:Port) as Player 2 and start the game
start_client(Host, Port) :-
    write("Connecting to "), write(Host), write(":"), write(Port), nl,
    setup_call_cleanup(
        (
            tcp_connect(Host:Port, StreamPair, [])
        ),
        game_loop(StreamPair, o),
        cleanup(StreamPair)
    ).

plays_first(x, false).
plays_first(o, true).

% From this predicate down, everything is symmetric for the server and client.
% The start of game loop announces the players game mark, determines who plays first
% and then enters the corresponding phase of the game loop.
game_loop(StreamPair, Player) :-
    write("+------------------------+"), nl,
    write("| You are playing with "), writetile(Player), write(" |"), nl,
    write("+------------------------+"), nl,
    plays_first(Player, Plays),
    game_loop_inner(StreamPair, Player, [[x]], Plays).

% last argument says if it is this player's turn
% +StreamPair = the connection
% +Player = this player's mark
% +BoardState = the current state of the board
% +IsMyTurn = true if it is this player's turn
game_loop_inner(StreamPair, Player, BoardState, false) :-
    opposite(Player, Opponent),
    write("Waiting for "), writetile(Opponent), write(" to make a move"), nl,

    % read the 4 bytes with the opponent's move and process the message
    read_string(StreamPair, 4, U),
    string_codes(U, Codes),
    process_message(StreamPair, Player, BoardState, Codes).

game_loop_inner(StreamPair, Player, BoardState, true) :-
    % expand the board state, print it and get the move
    expand_board_state(BoardState, ExpandedBoardState),
    nl, print_board(ExpandedBoardState, MaxX, MaxY),
    get_move(ExpandedBoardState, MaxX, MaxY, Player, X, Y, UpdatedBoard),

    % trim the board state back as needed, send the move to the opponent
    trim_board_state(UpdatedBoard, X, Y, NewBoardState),
    A is X // 256, B is X mod 256, C is Y // 256, D is Y mod 256,
    string_codes(Msg, [A,B,C,D]),
    write(StreamPair, Msg),
    flush_output(StreamPair),

    % loop the game loop, now waiting for the opponent to make a move
    check_win_and_loop(StreamPair, Player, NewBoardState, false).

% +StreamPair, +Player, +BoardState, +MessageBytes
% Match the case where we correctly received 4 bytes. This might fail in the set_board_tile
% predicate if the coordinates were out of range, then this fails and Prolog retries this predicate,
% which prints the error message and shuts down.
process_message(StreamPair, Player, BoardState, [A,B,C,D]) :-
    X is A * 256 + B,
    Y is C * 256 + D,
    expand_board_state(BoardState, ExpandedBoardState),
    opposite(Player, Opponent),
    set_board_tile(ExpandedBoardState, X, Y, Opponent, OldValue, UpdatedBoard),
    (OldValue = e -> true ; write("Is the other player cheating, or is this implementation broken? Opponent moved on a non-empty tile"), nl),
    trim_board_state(UpdatedBoard, X, Y, NewBoardState),

    % loop the game loop, now waiting for this player to make a move
    check_win_and_loop(StreamPair, Player, NewBoardState, true).

process_message(_, _, _, []) :- !,
    write("The opponent disconnected."), nl.

% The general case where we received too few bytes or the message was invalid.
process_message(_, _, _, L) :-
    write("Received "),
    length(L, Len), write(Len),
    write(" bytes with invalid move: "), write(L),
    write_error_coords(L), write(". The connection maybe got corrupted?"), nl.

% if the invalid move was full 4 bytes, try to interpret them as coordinates
% to show how out of range they would be
write_error_coords([A,B,C,D]) :- !,
    X is A * 256 + B,
    Y is C * 256 + D,
    number_lettercodes(X, XCodes),
    string_codes(XStr, XCodes),
    write(" (this would mean coordinates "), write(XStr), write(Y), write(")").
% if it was shorter, do not print anything extra
write_error_coords(_).

check_win_and_loop(StreamPair, Player, NewBoardState, IsMyTurn) :-
    (
        check_win(NewBoardState, Winner)
    ->
        nl, nl, print_board(NewBoardState, _, _),
        write("Player "), writetile(Winner), write(" wins!"), nl
    ;
        game_loop_inner(StreamPair, Player, NewBoardState, IsMyTurn)
    ).
