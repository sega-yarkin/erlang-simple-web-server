
HandleRequest = fun(Sock) ->
    gen_tcp:recv(Sock, 0),
    gen_tcp:send(Sock, ["HTTP/1.0 200 OK\r\n"]),
    gen_tcp:close(Sock)
end.

AcceptLoop = fun(LSock, Loop) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    HandleRequest(Sock),
    Loop(LSock, Loop)
end.

{ok, LSock} = gen_tcp:listen(8080,
        [{packet, http}, {active, false}, {backlog, 1024}]).
AcceptLoop(LSock, AcceptLoop).
