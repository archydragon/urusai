%%% Commands available by sending private messages by bot owner.
%%%
-module (urusai_xmpp_commands).

-export ([cmd/2]).

%% Just ping-pong
cmd(<<"ping">>, []) ->
    {ok, <<"pong">>};
%% Change status message
cmd(<<"status">>, Message) ->
    gen_server:call(urusai_xmpp, {status, Message});
%% Displays possible actions with owner management
cmd(<<"owner">>, []) ->
    {ok, <<"Allowed actions: \n\tlist\n\tadd <JID>\n\tdel <JID>">>};
%% Owner management — list, add, delete
cmd(<<"owner">>, [Params]) ->
    OKey = <<"owners">>,
    [Action | Tail] = binary:split(Params, <<" ">>),
    Reply = case Action of
        <<"list">> ->
            io_lib:format("~p", [urusai_db:get(OKey)]);
        <<"add">> when Tail =/= [] ->
            % TODO: disallow possibility to add a single JID multiple times
            [JID] = Tail,
            urusai_db:set(OKey, lists:append(urusai_db:get(OKey), [binary_to_list(JID)])),
            <<JID/binary, " added to owners.">>;
        <<"del">> when Tail =/= [] ->
            [JID] = Tail,
            case urusai_config:get(common, owner) =:= binary_to_list(JID) of
                true ->
                    <<"The owner set in configuration file cannot be deleted.">>;
                _  ->
                    urusai_db:set(OKey, lists:delete(binary_to_list(JID), urusai_db:get(OKey))),
                    <<JID/binary, " removed from owners.">>
            end;
        _ ->
            <<"Invalid action. Allowed actions: \n\tlist\n\tadd <JID>\n\tdel <JID>">>
    end,
    {ok, Reply};
%% MUC management — join, leave, change nickname
cmd(<<"muc">>, [Params]) ->
    [Action | [Tail]] = binary:split(Params, <<" ">>),
    [Muc | P] = binary:split(Tail, <<" ">>),
    gen_server:call(urusai_xmpp, case Action of
        <<"join">>  -> {muc_join, Muc, P};
        <<"pjoin">> -> {muc_join_protected, Muc, P};
        <<"leave">> -> {muc_leave, Muc};
        <<"nick">>  -> {muc_nick, Muc, P};
        % TODO: implement kick and ban triggers ^_^
        _           -> {ok, <<"Bad parameters.">>}
    end);
%% List of loaded plugin triggers
cmd(<<"plugins">>, []) ->
    {ok, <<"Allowed actions:\n\tlist\n\treload">>};
%% Reload plugins
cmd(<<"plugins">>, [Action]) ->
    case Action of
        <<"list">>   -> {ok, io_lib:format("~p", [urusai_plugin:plugins()])};
        <<"reload">> -> urusai_plugin:reload(), {ok, <<"Plugins reloaded.">>};
        _            -> {ok, <<"Bad action.">>}
    end;
%% Make possible to owners run PM plugins
cmd(<<"exec">>, Cmd) ->
    So = urusai_plugin:match(private, <<"OWNER@NO/WHERE">>, [], Cmd),
    {ok, io_lib:format("~p", [So])};
cmd(_, _) ->
    error.
