-module(web_server).
-export([start/0, start/1]).

%%
%% Interface
%%
start() -> start(8080).
start(Port) ->
    do_start(Port).

%%
%% Internal
%%
do_start(Port) ->
    Opts = [ {packet, http},
             {active, false},
             {backlog, 1024} ],
    {ok, LSock} = gen_tcp:listen(Port, Opts),
    accept_loop(LSock).


accept_loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    handle_request(Sock),
    accept_loop(LSock).


handle_request(Sock) ->
    gen_tcp:recv(Sock, 0),
    gen_tcp:send(Sock, ["HTTP/1.0 200 OK\r\n"]),
    gen_tcp:close(Sock).
