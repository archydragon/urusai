%%% Commands available by sending private messages by bot owner.
%%%
-module (urusai_xmpp_cmd).

-export ([cmd/2]).

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
        <<"join">>  -> {muc_join, Muc, P};
        <<"pjoin">> -> {muc_join_protected, Muc, P};
        <<"leave">> -> {muc_leave, Muc};
        <<"nick">>  -> {muc_nick, Muc, P};
        % TODO: implement kick and ban triggers ^_^
        _           -> {ok, <<"Bad parameters.">>}
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
        <<"list">>   -> {ok, io_lib:format("~p", [urusai_plugin:plugins()])};
        <<"reload">> -> urusai_plugin:reload(), {ok, <<"Plugins reloaded.">>};
        _            -> {ok, <<"Bad action.">>}
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
