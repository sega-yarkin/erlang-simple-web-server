
{ok, LSock} = gen_tcp:listen(8080, [{packet, http}, {active, false}]).
{ok, Sock} = gen_tcp:accept(LSock).
gen_tcp:recv(Sock, 0).
gen_tcp:send(Sock, ["HTTP/1.0 200 OK\r\n"]).
gen_tcp:close(Sock).
