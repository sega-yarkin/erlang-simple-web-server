-module(web_server).
-export([start/0, start/1]).

-record(request, {
    method  = undef,
    path    = undef,
    version = undef,
    headers = #{}
}).

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
    case parse_request(Sock) of
        ignore -> ok;
        {ok, #request{path = Path}} ->
            gen_tcp:send(Sock, [
                "HTTP/1.0 200 OK\r\n",
                "\r\n",
                "The page '", Path, "' is cool, isn't it?"
            ]);
        {error, Error} ->
            io:format("Error: ~p", [Error]),
            gen_tcp:send(Sock, [
                "HTTP/1.0 500 Internal Error\r\n"
            ])
    end,
    gen_tcp:close(Sock).


parse_request(Sock) ->
    parse_request(Sock, #request{}).

parse_request(Sock, Req) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, {http_request, Method, {abs_path, Path}, Ver}} ->
            Req1 = Req#request{method  = Method,
                               path    = Path,
                               version = Ver},
            parse_request(Sock, Req1);
        {ok, {http_header, _, Name, _, Value}} ->
            Headers = Req#request.headers,
            Headers1 = Headers#{Name => Value},
            Req1 = Req#request{headers = Headers1},
            parse_request(Sock, Req1);
        {ok, http_eoh} ->
            {ok, Req};
        %% Error handling
        {ok, {http_error, Error}} -> {error, Error};
        {error, closed} -> ignore;
        {error, Error } -> {error, Error};
        Data            -> {error, Data}
    end.
