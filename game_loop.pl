:- use_module(library(socket)).
:- use_module(library(readutil)).
:- include('piskvorky.pl').

cleanup(StreamPair) :-
    write("Cleaning up"), nl,
    close(StreamPair).

shared_setup :-
    % the signal handler doesn't work, the exception is swallowed with just a warning:
    % foreign predicate <...> did not clear exception
    write("Setting up signal handler"), nl.
    % on_signal(int, _, throw).


start_server(Port) :-
    write("Starting server on port "), write(Port), nl,
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    tcp_open_socket(Socket, AcceptFd),
    tcp_accept(AcceptFd, GameSocket, PeerIP),
    write("Accepted connection from "), write(PeerIP), nl,
    % close the server socket, do not listen for more connections
    close(AcceptFd),
    setup_call_cleanup(
        (
            tcp_open_socket(GameSocket, StreamPair),
            shared_setup
        ),
        game_loop(StreamPair, x),
        cleanup(StreamPair)
    ).

start_client(Host, Port) :-
    write("Connecting to "), write(Host), write(":"), write(Port), nl,
    setup_call_cleanup(
        (
            tcp_connect(Host:Port, StreamPair, []),
            shared_setup
        ),
        game_loop(StreamPair, o),
        cleanup(StreamPair)
    ).

plays_first(x, false).
plays_first(o, true).

game_loop(StreamPair, Player) :-
    write("start of game loop"), nl,
    bagof(X, stream_property(StreamPair, X), Props),
    write("Stream properties: "), write(Props), nl,
    plays_first(Player, Plays),
    game_loop_inner(StreamPair, Player, [[x]], Plays),
    write("end of game loop"), nl.

game_loop_inner(StreamPair, Player, BoardState, false) :-
    write("Waiting for "), write(Player), write(" to make a move"), nl,
    read_string(StreamPair, 4, U),
    atom_codes(U, Codes),
    process_message(StreamPair, Player, BoardState, Codes).

game_loop_inner(StreamPair, Player, BoardState, true) :-
    print_board(BoardState, MaxX, MaxY),
    write("Choose your move: "),
    get_coords(X, Y, MaxX, MaxY),
    A is X // 256, B is X mod 256, C is Y // 256, D is Y mod 256,
    atom_codes(Msg, [A,B,C,D]),
    write(StreamPair, Msg),
    flush_output(StreamPair),
    write("Move sent"), nl,
    game_loop_inner(StreamPair, Player, BoardState, false).

process_message(_, _, _, []).
process_message(StreamPair, Player, BoardState, [255,255,255,255]) :-
    % ping message, also send a ping
    atom_codes(PING_MSG, [255,255,255,255]),
    write(StreamPair, PING_MSG),
    flush_output(StreamPair),
    game_loop_inner(StreamPair, Player, BoardState, false).

process_message(StreamPair, Player, BoardState, [A,B,C,D]) :-
    X is A * 256 + B,
    Y is C * 256 + D,
    write("Received move: "), write(X), write(", "), write(Y), nl,
    game_loop_inner(StreamPair, Player, BoardState, true).

process_message(_, _, L) :-
    write("Received "),
    length(L, Len), write(Len),
    write(" orphan bytes, connection maybe got corrupted?"), nl.
