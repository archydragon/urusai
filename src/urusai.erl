-module (urusai).

-export ([start/0, stop/0]).

start() ->
    lager:start(),
    ok = application:start(pooler),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(sasl),
    ok = application:start(ssl),
    ok = application:start(exmpp),
    case application:start(urusai) of
        ok ->
            application:start(ranch),
            application:start(cowboy),
            urusai_http:start();
        {error, Reason} ->
            lager:error("Failed to start urusai: ~p", [Reason]),
            stop()
    end.

stop() ->
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(urusai),
    application:stop(exmpp),
    application:stop(ssl),
    application:stop(sasl),
    application:stop(public_key),
    application:stop(crypto),
    application:stop(pooler).
