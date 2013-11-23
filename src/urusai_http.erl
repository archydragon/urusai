-module (urusai_http).

-export ([start/0]).

start() ->
    Dispatch = cowboy_router:compile([
        %% {URIHost, list({URIPath, Handler, Opts})}
        {'_', [
            {urusai_config:get(http, path), urusai_http_handler, []}
        ]}
    ]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(http, 10,
        [{port, urusai_config:get(http, port)}],
        [{env, [{dispatch, Dispatch}]}]
    ).
