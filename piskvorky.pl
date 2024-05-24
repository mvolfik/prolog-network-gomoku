start_server(Port) :-
    write("Starting server on port "), write(Port), nl.

start_client(Host, Port) :-
    write("Starting client, connecting to "), write(Host), write(":"), write(Port), nl.
