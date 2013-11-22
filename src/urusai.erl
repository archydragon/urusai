-module (urusai).

-export ([start/0, stop/0]).

start() ->
    lager:start(),
    application:start(pooler),
    application:start(crypto),
    application:start(public_key),
    application:start(sasl),
    application:start(ssl),
    application:start(exmpp),
    application:start(urusai).

stop() ->
    application:stop(urusai),
    application:stop(exmpp),
    application:stop(ssl),
    application:stop(sasl),
    application:stop(public_key),
    application:stop(crypto),
    application:stop(pooler),
    lager:stop().
