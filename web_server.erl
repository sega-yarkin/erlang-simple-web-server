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
    %% DB
    Views = ets:new(views, [set, public]),
    %% Net
    Opts = [ {packet, http},
             {active, false},
             {backlog, 1024} ],
    {ok, LSock} = gen_tcp:listen(Port, Opts),
    accept_loop(LSock, Views).


accept_loop(LSock, Views) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> handle_loop(Sock, Views) end),
    accept_loop(LSock, Views).


handle_loop(Sock, Views) ->
    case handle_request(Sock, Views) of
        loop  -> handle_loop(Sock, Views);
        close -> gen_tcp:close(Sock)
    end.


handle_request(Sock, Views) ->
    case parse_request(Sock) of
        ignore -> close;
        {ok, Req = #request{}} ->
            {Resp, KA} = get_response(Req, Views),
            gen_tcp:send(Sock, Resp),
            case KA of
                true  -> loop;
                false -> close
            end;
        {error, Error} ->
            io:format("Error: ~p", [Error]),
            gen_tcp:send(Sock, [
                "HTTP/1.0 500 Internal Error\r\n"
            ]),
            close
    end.


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


get_views(Path, Views) ->
    ets:update_counter(Views, Path, 1, {Path, 0}).


format_views(1) -> ["There is 1 view already!"];
format_views(N) -> ["There are ", integer_to_list(N), " views already!"].


get_body(#request{path = Path}, Views) ->
    ViewCnt = get_views(Path, Views),
    Body = [ "The page '", Path,
             "' is cool, isn't it?\r\n",
             format_views(ViewCnt)],
    {Body, iolist_size(Body)}.


is_keepalive(#request{headers = #{'Connection' := Conn}}) ->
    string:to_lower(Conn) =:= "keep-alive";
is_keepalive(_) -> false.


get_headers(KA, CLen) ->
    Conn = case KA of
        true  -> {"Connection", "keep-alive"};
        false -> {"Connection", "close"}
    end,
    ContLen = {"Content-Length",
                integer_to_list(CLen)},
    Headers = [ Conn, ContLen ],
    [ [Name, ": ", Value, "\r\n"]
        || {Name, Value} <- Headers ].


get_response(Req, Views) ->
    {Body, CLen} = get_body(Req, Views),
    IsKA = is_keepalive(Req),
    Headers = get_headers(IsKA, CLen),
    Resp = [ "HTTP/1.0 200 OK\r\n",
             Headers,
             "\r\n",
             Body ],
    {Resp, IsKA}.
