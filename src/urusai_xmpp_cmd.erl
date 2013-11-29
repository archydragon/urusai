%%% Commands available by sending private messages by bot owner.
%%%
-module (urusai_xmpp_cmd).

-export ([cmd/2]).

%% Help
cmd(<<"help">>, []) ->
    R = <<
        "Allowed commands:\n",
        "\t\"h[elp]\" — this help\n",
        "\t\"p[ing]\" — pong!\n",
        "\t\"s[tatus] <YOUR_STATUS_MESSAGE>\" — update status message\n",
        "\t\"o[wner] l[ist]\" — list of bot's owners\n",
        "\t\"o[wner] a[dd] <JID>\" — add <JID> to owners list\n",
        "\t\"o[wner] d[el] <JID>\" — remove <JID> from owners list\n",
        "\t\"m[uc] j[oin] <MUC_ADDRESS> [<NICK>]\" — join MUC, custom nick may be set on this stage\n",
        "\t\"m[uc] pj[oin] <MUC_ADDRESS> <PASSWORD>\" — join password protected MUC\n",
        "\t\"m[uc] l[eave] <MUC_ADDRESS>\" — leave MUC\n",
        "\t\"m[uc] n[ick] <MUC_ADDRESS>\" — change bot's shown nick for this MUC\n",
        "\t\"pl[ugins] l[ist]\" — list of loaded plugins' triggers information\n",
        "\t\"pl[ugins] r[eload]\" — reload plugins\n",
        "\t\"g[et] <KEY>\" — get the value of <KEY> field from the database\n",
        "\t\"e[xec] <COMMAND>\" — execute private message plugin command\n"
    >>,
    {ok, R};
cmd(<<"h">>, []) ->
    cmd(<<"help">>, []);
%% Just ping-pong
cmd(<<"ping">>, []) ->
    {ok, <<"pong">>};
cmd(<<"p">>, []) ->
    cmd(<<"ping">>, []);
%% Change status message
cmd(<<"status">>, Message) ->
    gen_server:call(urusai_xmpp, {status, Message});
cmd(<<"s">>, Message) ->
    cmd(<<"status">>, Message);
%% Displays possible actions with owner management
cmd(<<"owner">>, []) ->
    {ok, owner(<<"help">>, [])};
cmd(<<"owner">>, [Params]) ->
    [Action | Tail] = binary:split(Params, <<" ">>),
    {ok, owner(Action, Tail)};
cmd(<<"o">>, []) ->
    cmd(<<"owner">>, []);
cmd(<<"o">>, [Params]) ->
    cmd(<<"owner">>, [Params]);
%% MUC management — join, leave, change nickname
cmd(<<"muc">>, [Params]) ->
    [Action | [Tail]] = binary:split(Params, <<" ">>),
    [Muc | P] = binary:split(Tail, <<" ">>),
    gen_server:call(urusai_xmpp, case Action of
        X when X == <<"join">> orelse X == <<"j">> ->
            {muc_join, Muc, P};
        X when X == <<"pjoin">> orelse X == <<"pj">> ->
            {muc_join_protected, Muc, P};
        X when X == <<"leave">> orelse X == <<"l">> ->
            {muc_leave, Muc};
        X when X == <<"nick">> orelse X == <<"n">> ->
            {muc_nick, Muc, P};
        % TODO: implement kick and ban triggers ^_^
        _ ->
            {ok, <<"Bad parameters.">>}
    end);
cmd(<<"m">>, [Params]) ->
    cmd(<<"muc">>, [Params]);
%% List of loaded plugin triggers
cmd(<<"plugins">>, []) ->
    {ok, <<"Allowed actions:\n\tlist\n\treload">>};
cmd(<<"pl">>, []) ->
    cmd(<<"plugins">>, []);
%% Reload plugins
cmd(<<"plugins">>, [Action]) ->
    case Action of
        X when X == <<"list">> orelse X == <<"l">> ->
            {ok, io_lib:format("~p", [urusai_plugin:plugins()])};
        X when X == <<"reload">> orelse X == <<"r">> ->
            urusai_plugin:reload(), {ok, <<"Plugins reloaded.">>};
        _ ->
            {ok, <<"Bad action.">>}
    end;
cmd(<<"pl">>, [Action]) ->
    cmd(<<"plugins">>, [Action]);
%% Get database record
cmd(<<"get">>, [Params]) ->
    {ok, list_to_binary(io_lib:format("~p", [urusai_db:get(Params)]))};
cmd(<<"g">>, [Params]) ->
    cmd(<<"get">>, [Params]);
%% Make possible to owners run PM plugins
cmd(<<"exec">>, Cmd) ->
    So = urusai_plugin:match(private, <<"OWNER@NO/WHERE">>, [], Cmd),
    {ok, io_lib:format("~p", [So])};
cmd(<<"e">>, Cmd) ->
    cmd(<<"exec">>, Cmd);
cmd(_, _) ->
    error.

%%% ---------------------------------
%%% Internal functions
%%% ---------------------------------

%% Owner management
owner(<<"help">>, []) ->
    <<"Allowed actions: \n\tlist\n\tadd <JID>\n\tdel <JID>">>;
owner(<<"list">>, []) ->
    io_lib:format("~p", [urusai_db:get(<<"owners">>)]);
owner(<<"add">>, [Jid]) when Jid =/= <<"">> ->
    urusai_db:set(<<"owners">>, lists:usort(lists:append(urusai_db:get(<<"owners">>), [binary_to_list(Jid)]))),
    <<Jid/binary, " added to owners.">>;
owner(<<"del">>, [Jid]) when Jid =/= <<"">> ->
    case urusai_config:get(common, owner) =:= binary_to_list(Jid) of
        true ->
            <<"The owner set in configuration file cannot be deleted.">>;
        _  ->
            urusai_db:set(<<"owners">>, lists:delete(binary_to_list(Jid), urusai_db:get(<<"owners">>))),
            <<Jid/binary, " removed from owners.">>
    end;
owner(_, _) ->
    H = owner(<<"help">>, []),
    <<"Invalid action.\n", H/binary>>.
