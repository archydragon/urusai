-module (urusai).

-export ([start/0, stop/0]).

start() ->
    lager:start(),
    application:start(pooler),
    application:start(sasl),
    application:start(exmpp),
    application:start(urusai).

stop() ->
    application:stop(urusai),
    application:stop(exmpp),
    application:stop(sasl),
    application:stop(pooler),
    lager:stop().
