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
        "\t\"v[ersion] — bot version information\n",
        "\t\"u[ptime] — bot and system uptime\n"
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
        "\t\"e[xec] <COMMAND>\" — execute private message plugin command\n",
        "\t\"die\" — stop bot\n"
    >>,
    {ok, R};
cmd(<<"h">>, []) ->
    cmd(<<"help">>, []);
%% Just ping-pong
cmd(<<"ping">>, []) ->
    {ok, <<"pong">>};
cmd(<<"p">>, []) ->
    cmd(<<"ping">>, []);
%% Version info
cmd(<<"v">>, []) ->
    cmd(<<"version">>, []);
cmd(<<"version">>, []) ->
    Version = urusai_xmpp:version(),
    BuildInfo = case file:read_file(".build-meta") of
        {error, _} -> 
            <<>>;
        {ok, Content} ->
            [BuildDate, BuildRev, _] = binary:split(Content, <<"\n">>, [global]),
            list_to_binary(io_lib:format(" (built ~s from git rev ~s)", [BuildDate, BuildRev]))
    end,
    ErlVersion = list_to_binary(erlang:system_info(system_version)),
    PyVersion = list_to_binary(os:cmd("python --version")),
    Reply = <<"Urusai ", Version/binary, BuildInfo/binary, "\n", ErlVersion/binary, PyVersion/binary>>,
    {ok, Reply};
%% Get uptime details
cmd(<<"u">>, []) ->
    cmd(<<"uptime">>, []);
cmd(<<"uptime">>, []) ->
    RawBotUptime = calendar:time_difference(urusai_db:get(<<"started">>), calendar:now_to_local_time(now())),
    RawSysUptime = case os:type() of
        {unix, linux} ->
            calendar:seconds_to_daystime(
                round(list_to_float(lists:nth(1, string:tokens(os:cmd("cat /proc/uptime"), " ")))));
        {unix, OS} when OS =:= darwin orelse OS =:= freebsd ->
            BootTime = list_to_integer(string:strip(
                lists:nth(4, string:tokens(os:cmd("sysctl -n kern.boottime"), " ")), both, $,)),
            {N1, N2, _} = erlang:now(),
            calendar:seconds_to_daystime((N1 * 1000000 + N2) - BootTime);
        {win32, _} ->
            BootUpTime = os:cmd("wmic os get lastbootuptime"),
            {match, [Y, M, D, Hh, Mm, Ss]} = 
                re:run(BootUpTime, "\n([0-9]{4})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2})([0-9]{2}).*",
                [{capture, all_but_first, list}]),
            calendar:time_difference(
                {{list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
                 {list_to_integer(Hh), list_to_integer(Mm), list_to_integer(Ss)}},
                calendar:now_to_local_time(now()));
        {_, _} ->
            {0, {0, 0, 0}}
    end,
    Bot = time_fmt(RawBotUptime),
    Sys = time_fmt(RawSysUptime),
    {ok, <<"Bot uptime: ", Bot/binary, "\nSystem uptime: ", Sys/binary>>};
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
    case Action of
        X when X == <<"join">> orelse X == <<"j">> ->
            gen_server:call(urusai_xmpp, {muc_join, Muc, P});
        X when X == <<"pjoin">> orelse X == <<"pj">> ->
            gen_server:call(urusai_xmpp, {muc_join_protected, Muc, P});
        X when X == <<"leave">> orelse X == <<"l">> ->
            gen_server:call(urusai_xmpp, {muc_leave, Muc});
        X when X == <<"nick">> orelse X == <<"n">> ->
            gen_server:call(urusai_xmpp, {muc_nick, Muc, P});
        % TODO: implement kick and ban triggers ^_^
        _ ->
            {ok, <<"Bad parameters.">>}
    end;
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
cmd(<<"die">>, []) ->
    erlang:send_after(1000, urusai_xmpp, die),
    {ok, <<"Goodbye.">>};
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
owner(<<"l">>, []) ->
    owner(<<"list">>, []);
owner(<<"add">>, [Jid]) when Jid =/= <<"">> ->
    urusai_db:set(<<"owners">>, lists:usort(lists:append(urusai_db:get(<<"owners">>), [binary_to_list(Jid)]))),
    <<Jid/binary, " added to owners.">>;
owner(<<"a">>, [Jid]) ->
    owner(<<"add">>, [Jid]);
owner(<<"del">>, [Jid]) when Jid =/= <<"">> ->
    case urusai_config:get(common, owner) =:= binary_to_list(Jid) of
        true ->
            <<"The owner set in configuration file cannot be deleted.">>;
        _  ->
            urusai_db:set(<<"owners">>, lists:delete(binary_to_list(Jid), urusai_db:get(<<"owners">>))),
            <<Jid/binary, " removed from owners.">>
    end;
owner(<<"d">>, [Jid]) ->
    owner(<<"del">>, [Jid]);
owner(_, _) ->
    H = owner(<<"help">>, []),
    <<"Invalid action.\n", H/binary>>.

%% Format uptime
time_fmt({Days, {Hours, Minutes, Seconds}}) ->
    list_to_binary(io_lib:format("~b days ~2..0w:~2..0w:~2..0w", [Days, Hours, Minutes, Seconds])).
