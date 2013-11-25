%%% Commands available by sending private messages by MUC owner.
%%%
-module (urusai_xmpp_cmd_muc).

-export ([cmd/3]).

cmd(_, <<"ping">>, []) ->
    {ok, <<"pong">>};
cmd(Muc, <<"w">>, []) ->
    {ok, <<"You're from ", Muc/binary>>};
cmd(Muc, <<"leave">>, []) ->
    gen_server:cast(urusai_xmpp, {muc_leave, Muc}),
    {ok, <<"As you wish.">>};
cmd(_, _, _) ->
    error.
