%%% Cowboy starter
%%%
-module (urusai_http).

-export ([start/0]).

start() ->
    %% If you need to start HTTP API on the other port and make it using the
    %% other path, update configuration file.
    Dispatch = cowboy_router:compile([
        {'_', [
            {urusai_config:get(http, path), urusai_http_handler, []}
        ]}
    ]),
    cowboy:start_http(http, 10,
        [{port, urusai_config:get(http, port)}],
        [{env, [{dispatch, Dispatch}]}]
    ).
