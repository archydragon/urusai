%%% Commands available by sending private messages by MUC owner.
%%%
-module (urusai_xmpp_cmd_muc).

-export ([cmd/3]).

cmd(_, <<"ping">>, []) ->
    {ok, <<"pong">>};
cmd(_, <<"p">>, []) ->
    cmd([], <<"ping">>, []);
cmd(Muc, <<"w">>, []) ->
    {ok, <<"You're from ", Muc/binary>>};
cmd(Muc, <<"leave">>, []) ->
    gen_server:cast(urusai_xmpp, {muc_leave, Muc}),
    {ok, <<"As you wish.">>};
cmd(Muc, <<"l">>, []) ->
    cmd(Muc, <<"leave">>, []);
cmd(_, <<"plugins">>, []) ->
    {ok, plug([], <<"help">>, [])};
cmd(_, <<"pl">>, []) ->
    cmd([], <<"plugins">>, []);
cmd(Muc, <<"plugins">>, [Params]) ->
    [Action | Tail] = binary:split(Params, <<" ">>),
    {ok, plug(Muc, Action, Tail)};
cmd(Muc, <<"pl">>, [Params]) ->
    cmd(Muc, <<"plugins">>, [Params]);
cmd(_, <<"http">>, []) ->
    {ok, <<"Allowed actions:\n\tstate\n\ttoggle">>};
cmd(_, <<"h">>, []) ->
    cmd([], <<"http">>, []);
cmd(Muc, <<"http">>, [Params]) ->
    {ok, http(Muc, Params)};
cmd(Muc, <<"h">>, [Params]) ->
    cmd(Muc, <<"http">>, [Params]);
cmd(_, _, _) ->
    error.

%%% ---------------------------------
%%% Internal functions
%%% ---------------------------------

%% Manage plugins for MUC
plug(_, <<"help">>, []) ->
    <<"Allowed actions:\n\tlist\n\ttoggle <PLUGIN>">>;
plug(Muc, <<"list">>, []) ->
    MucK = <<"muc_plugins_", Muc/binary>>,
    AllPlugins = urusai_plugin:plugins(mucmessage),
    EnabledPlugins = urusai_db:get(MucK),
    Out = [ plugin_line(P, lists:member(P, EnabledPlugins)) || P <- AllPlugins ],
    list_to_binary(Out);
plug(Muc, <<"toggle">>, [Plugin]) when Plugin =/= <<>> ->
    MucK = <<"muc_plugins_", Muc/binary>>,
    P = list_to_atom(binary_to_list(Plugin)),
    AllPlugins = urusai_plugin:plugins(mucmessage),
    EnabledPlugins = urusai_db:get(MucK),
    case {lists:member(P, EnabledPlugins), lists:member(P, AllPlugins)} of
        {true, _} ->
            urusai_db:set(MucK, lists:delete(P, urusai_db:get(MucK))),
            list_to_binary(io_lib:format("Plugin '~s' has been disabled for the room ~s", [P, Muc]));
        {false, true} ->
            urusai_db:set(MucK, lists:append(urusai_db:get(MucK), [P])),
            list_to_binary(io_lib:format("Plugin '~s' has been enabled for the room ~s", [P, Muc]));
        {false, false} ->
            list_to_binary(io_lib:format("There is no plugin '~s' found.", [P]))
    end;
plug(Muc, <<"l">>, []) ->
    plug(Muc, <<"list">>, []);
plug(Muc, <<"t">>, Plugin) ->
    plug(Muc, <<"toggle">>, Plugin);
plug(_, _, _) ->
    H = plug([], <<"help">>, []),
    <<"Invalid action.\n", H/binary>>.

plugin_line(Plugin, true) ->
    list_to_binary(io_lib:format("\n[X] ~s", [Plugin]));
plugin_line(Plugin, false) ->
    list_to_binary(io_lib:format("\n[_] ~s", [Plugin])).

%% Manage HTTP API for MUC
http([], <<"help">>) ->
    <<"Allowed actions:\n\tstate\n\ttoggle">>;
http(Muc, <<"state">>) ->
    case lists:member(Muc, urusai_db:get(<<"muc_http_enabled">>)) of
        true  -> <<"Sending messages via HTTP API to this MUC is enabled.">>;
        false -> <<"Sending messages via HTTP API to this MUC is disabled.">>
    end;
http(Muc, <<"toggle">>) ->
    EnabledList = urusai_db:get(<<"muc_http_enabled">>),
    case lists:member(Muc, EnabledList) of
        true ->
            urusai_db:set(<<"muc_http_enabled">>, lists:delete(Muc, EnabledList)),
            <<"HTTP API has been disabled for MUC ", Muc/binary>>;
        false ->
            urusai_db:set(<<"muc_http_enabled">>, lists:append(EnabledList, [Muc])),
            <<"HTTP API has been enabled for MUC ", Muc/binary>>
    end;
http(Muc, <<"s">>) ->
    http(Muc, <<"state">>);
http(Muc, <<"t">>) ->
    http(Muc, <<"toggle">>);
http(_, _) ->
    H = http([], <<"help">>),
    <<"Invalid action.\n", H/binary>>.
