%%% XMPP client. Need I say more?
%%%
-module (urusai_xmpp).

-behaviour (gen_server).

-define (SERVER, ?MODULE).
-define (CMD, urusai_xmpp_cmd).
-define (CMD_MUC, urusai_xmpp_cmd_muc).

-include_lib ("deps/exmpp/include/exmpp.hrl").
-include_lib ("deps/exmpp/include/exmpp_client.hrl").

-record (muc_member, {
    nick = <<>>,
    jid = <<>>,
    affiliation = <<>>,
    role = <<>>
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([start_link/0, parse_packet/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    connect().
handle_call({muc_join, Muc, Params}, _From, State) ->
    {reply, muc_join(State, Muc, Params, []), State};
handle_call({muc_join_protected, Muc, Params}, _From, State) ->
    {reply, muc_join_protected(State, Muc, Params), State};
handle_call({muc_leave, Muc}, _From, State) ->
    {reply, muc_leave(State, Muc), State};
handle_call({muc_nick, Muc, Nick}, _From, State) ->
    {reply, muc_nick(State, Muc, Nick), State};
handle_call({status, Message}, _From, State) ->
    {reply, status_message(State, Message), State};
handle_call({api_message, Target, Body}, _From, State) ->
    {reply, api_message(State, Target, Body), State};
handle_call({api_plugin, Target, Body}, _From, State) ->
    {reply, api_plugin(State, Target, Body), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_session, Session}, _State) ->
    {noreply, Session};
handle_cast({muc_leave, Muc}, State) ->
    timer:sleep(1000),
    muc_leave(State, Muc),
    {noreply, State};
handle_cast({send_packet, Packet}, Session) ->
    send_packet(Session, Packet),
    {noreply, Session};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(stop, State) ->
    {stop, normal, shutdown_ok, State};
handle_info(Packet, Session) ->
    spawn_link(?MODULE, parse_packet, [Session, Packet]),
    {noreply, Session}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% XMPP client connector
connect() ->
    [GetUser, GetServer, Password, Resource, Port]
        = [ urusai_config:get(auth, K) || K <- [login, server, password, resource, port] ],

    [User | TServer] = string:tokens(GetUser, "@"),
    % If server is unset in configuration value, parse it from login string
    case GetServer of
        []                    -> [ConnServer] = TServer, Server = ConnServer;
        _ when TServer =/= [] -> ConnServer = GetServer, [Server] = TServer;
        _                     -> ConnServer = GetServer, Server = GetServer
    end,

    % TODO: implement SSL
    Session = exmpp_session:start(),
    JID = exmpp_jid:make(User, Server, Resource),
    lager:info("Trying to connect as ~s@~s to the server ~s:~B", [User, Server, ConnServer, Port]),
    urusai_db:set(<<"current_jid">>, JID), % required for alerts
    exmpp_session:auth_basic_digest(Session, JID, Password),
    ConnectMethod = case urusai_config:get(auth, ssl) of
        true  -> connect_SSL;
        false -> connect_TCP
    end,
    {ok, _Stream} = exmpp_session:ConnectMethod(Session, ConnServer, Port),
    exmpp_session:login(Session),
    lager:info("Connected."),
    update_status(Session),
    muc_autojoin(Session),
    gen_server:cast(?SERVER, {set_session, Session}),
    {ok, Session}.

%% Send formed package
send_packet(Session, Message) ->
    lager:debug("[~p] -> ~p", [Session, Message]),
    exmpp_session:send_packet(Session, Message).

%% Parse received XMPP packet
parse_packet(Session, Packet) ->
    lager:debug("[~p] <- ~p", [Session, Packet]),
    case Packet of
        % MUC message
        #received_packet{packet_type=message,
                                  raw_packet=Raw,
                                  type_attr=Type} when Type =:= "groupchat" ->
            handle_muc_message(Session, Raw);
        % Private message
        #received_packet{packet_type=message,
                                  raw_packet=Raw,
                                  type_attr=Type} when Type =/= "error" ->
            handle_private(Session, Raw);
        % IQ message
        #received_packet{packet_type=iq,
                                  raw_packet=Raw,
                                  type_attr=_Type} ->
            handle_iq(Session, Raw);
        % Presence
        Record when Record#received_packet.packet_type == 'presence' ->
            handle_presence(Session, Record, Record#received_packet.raw_packet);
        _Other ->
            lager:error("Unknown packet received: ~p", [Packet])
    end.

%% MUC message handler
handle_muc_message(Session, Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    [Muc | User] = binary:split(From, <<"/">>),
    handle_muc_message(Session, Packet, From, urusai_db:get(<<"muc_nick_", Muc/binary>>) =:= User).

handle_muc_message(_Session, _Packet, _From, true) ->
    ok;
handle_muc_message(Session, Packet, From, false) ->
    Me = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    Command = exmpp_message:get_body(Packet),
    Matched = urusai_plugin:match(mucmessage, From, get_real_jid(From), [Command]),
    case lists:filter(fun(X) -> X =/= none end, Matched) of
        []    -> ok;
        Other -> [send_packet(Session, make_muc_packet(Me, From, E)) || E <- Other]
    end.

%% Private message handler
handle_private(Session, Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    Me = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    IsMucUser = binary:match(From, <<"@conference.">>) =/= nomatch,
    handle_private(Session, Packet, is_owner(From, IsMucUser), IsMucUser, From, Me).

%% Parse private message if it has been sent by MUC owner
%% TODO: optimize ownership code
handle_private(Session, Packet, true, true, Target, Me) ->
    Command = exmpp_message:get_body(Packet),
    [Muc | _] = binary:split(Target, <<"/">>),
    [C | P] = binary:split(Command, <<" ">>),
    Body = case ?CMD_MUC:cmd(Muc, C, P) of
        {ok, Reply} -> Reply;
        error       -> <<"Bad command.">>
    end,
    send_packet(Session, make_private_packet(Me, Target, Body));
%% Parse private message if it has been sent by bot owner
handle_private(Session, Packet, true, false, Target, Me) ->
    Command = exmpp_message:get_body(Packet),
    [C | P] = binary:split(Command, <<" ">>),
    % In case owner want to execute some plugin command, he should send it with `exec ` prefix
    PBody = case ?CMD:cmd(C, P) of
        {ok, Reply} -> Reply;
        error       -> <<"Bad internal command.">>
    end,
    send_packet(Session, make_private_packet(Me, Target, PBody));
%% Or if it has been sent by somebody else
handle_private(Session, Packet, false, _, Target, Me) ->
    Command = exmpp_message:get_body(Packet),
    case urusai_plugin:match(private, Target, <<>>, [Command]) of
        [none] -> 
            send_packet(Session, make_private_packet(Me, Target, <<"No such command.">>));
        Replies ->
            [send_packet(Session, make_private_packet(Me, Target, E)) || E <- Replies]
    end.

%% IQ handler
handle_iq(_Session, _Packet) ->
    % TODO: implement %)
    ok.

%% Presence handler
handle_presence(Session, Packet, _Presence) ->
    case exmpp_jid:make({Conf, Serv, Nick} = Packet#received_packet.from) of
        JID ->
            C = <<Conf/binary, "@", Serv/binary>>,
            case _Type = Packet#received_packet.type_attr of
                "available" ->
                    muc_userjoined(C, Nick, Packet#received_packet.raw_packet);
                "unavailable" ->
                    muc_userleft(C, Nick, Packet#received_packet.raw_packet);
                "subscribe" ->
                    presence_subscribed(Session, JID),
                    presence_subscribe(Session, JID);
                "subscribed" ->
                    presence_subscribed(Session, JID),
                    presence_subscribe(Session, JID);
                "error" ->
                    M = <<Conf/binary, "@", Serv/binary>>,
                    urusai_db:set(<<"autojoin">>, lists:delete(M, urusai_db:get(<<"autojoin">>))),
                    alert(Session, <<"Failed to join MUC ", M/binary>>),
                    ok
            end
    end.

%% ----------------------------------------
%% Presence actions
%% ----------------------------------------

presence_subscribed(Session, Recipient) ->
    lager:info("Exchanging subscriptions with ~s", [Recipient]),
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    send_packet(Session, Presence).

%% ----------------------------------------
%% MUC actions
%% ----------------------------------------

make_muc_packet(Me, ConfUser, Body) ->
    BodyX = try
        exmpp_message:chat(Body)
    catch _:_ ->
        exmpp_message:chat(<<"Malformed packet body, possible error in some plugin output.">>)
    end,
    To = exmpp_jid:bare_to_list(exmpp_jid:bare(exmpp_jid:parse(ConfUser))),
    exmpp_xml:set_attribute(
        exmpp_xml:set_attribute(
            exmpp_xml:set_attribute(
                BodyX, <<"from">>, Me), <<"to">>, To), <<"type">>, <<"groupchat">>).

muc_join(Session, Muc, Params, PacketBody) ->
    OldNick = urusai_db:get(<<"muc_nick_", Muc/binary>>),
    Nick = case Params of
        [] when OldNick =:= [] -> list_to_binary(urusai_config:get(muc, default_nick));
        []                     -> OldNick;
        _                      -> list_to_binary(Params)
    end,
    Presence = muc_join_packet(Muc, Nick, PacketBody),
    send_packet(Session, Presence),
    lager:info("Joining MUC ~s as ~s", [Muc, Nick]),
    urusai_db:set(<<"autojoin">>, lists:usort(lists:append(urusai_db:get(<<"autojoin">>), [Muc]))),
    urusai_db:set(<<"muc_nick_", Muc/binary>>, Nick),
    urusai_db:set(<<"muc_password_", Muc/binary>>, PacketBody),
    urusai_db:set(<<"muc_users_", Muc/binary>>, []),
    {ok, <<"Presence sent.">>}.

muc_join_protected(_Session, _Muc, []) ->
    {ok, <<"Please specify the password for joining password protected MUC.">>};
muc_join_protected(Session, Muc, [Password]) ->
    P = exmpp_xml:set_children(exmpp_xml:element('http://jabber.org/protocol/muc', x),
        [exmpp_xml:set_cdata(exmpp_xml:element(password), Password)]),
    muc_join(Session, Muc, [], [P]).

muc_join_packet(Muc, Nick, Children) ->
    RP = exmpp_xml:set_attribute(
        exmpp_presence:set_status(exmpp_presence:available(), urusai_db:get(<<"status">>)),
        <<"to">>, <<Muc/binary, "/", Nick/binary>>),
    exmpp_xml:set_children(RP, Children).

muc_leave(Session, Muc) ->
    RP = exmpp_presence:set_status(exmpp_presence:unavailable(), "kthxbye"),
    Presence = exmpp_xml:set_attribute(RP, <<"to">>, <<Muc/binary>>),
    send_packet(Session, Presence),
    lager:info("Leaving MUC ~s", [Muc]),
    urusai_db:set(<<"autojoin">>, lists:delete(Muc, urusai_db:get(<<"autojoin">>))),
    {ok, <<"Presence sent.">>}.

muc_nick(Session, Muc, [Nick]) ->
    RP = exmpp_presence:set_status(exmpp_presence:available(), urusai_db:get(<<"status">>)),
    Presence = exmpp_xml:set_attribute(RP, <<"to">>, <<Muc/binary, "/", Nick/binary>>),
    send_packet(Session, Presence),
    lager:info("Changed nick at MUC ~s to ~s", [Muc, Nick]),
    urusai_db:set(<<"muc_nick_", Muc/binary>>, Nick),
    {ok, <<"Presence sent.">>}.

muc_autojoin(Session) ->
    [ muc_join(Session, A, [], urusai_db:get(<<"muc_password_", A/binary>>)) || A <- urusai_db:get(<<"autojoin">>) ].

muc_userjoined(Conf, Nick, Raw) ->
    muc_userjoined_save(Conf, Nick, muc_getdata(Raw)).

muc_userjoined_save(_Conf, _Nick, ok) ->
    ok;
muc_userjoined_save(Conf, Nick, {Jid, Affiliation, Role}) ->
    User = #muc_member{
        nick = Nick,
        jid = Jid,
        affiliation = Affiliation,
        role = Role
    },
    muc_presence_match(<<Conf/binary, "/", Nick/binary>>, Jid, "available", Affiliation, Role),
    MucK = <<"muc_users_", Conf/binary>>,
    urusai_db:set(MucK, lists:append(urusai_db:get(MucK), [User])),
    ok.

muc_userleft(Conf, Nick, Raw) ->
    muc_userleft_save(Conf, Nick, muc_getdata(Raw)).

muc_userleft_save(_Conf, _Nick, ok) ->
    ok;
muc_userleft_save(Conf, Nick, {Jid, _Affiliation, _Role}) ->
    muc_presence_match(<<Conf/binary, "/", Nick/binary>>, Jid, "unavailable", "", ""),
    MucK = <<"muc_users_", Conf/binary>>,
    urusai_db:set(MucK, lists:filter(fun(X) -> X#muc_member.nick =/= Nick end, urusai_db:get(MucK))),
    ok.

muc_getdata(Raw) ->
    case exmpp_xml:get_element(exmpp_xml:get_element(Raw, 'http://jabber.org/protocol/muc#user', x), item) of
        undefined ->
            ok;
        Data ->
            list_to_tuple([ exmpp_xml:get_attribute(Data, A, <<>>)
                || A <- [<<"jid">>, <<"affiliation">>, <<"role">>] ])
    end.

muc_presence_match(From, Jid, Presence, Affiliation, Role) ->
    Msg = list_to_binary(io_lib:format("~s|~s|~s", [Presence, Affiliation, Role])),
    Matched = urusai_plugin:match(mucpresence, From, Jid, [Msg]),
    case lists:filter(fun(X) -> X =/= none end, Matched) of
        []    -> ok;
        % Other -> [send_packet(Session, make_muc_packet(Me, From, E)) || E <- Other]
        Other -> [ gen_server:cast(?MODULE, {send_packet, make_muc_packet(current_jid(), From, E)}) || E <- Other ]
    end.

get_real_jid(MucJid) ->
    case binary:split(MucJid, <<"/">>) of
        [MucJid] ->
            [];
        [Conf, Nick] ->
            MucK = <<"muc_users_", Conf/binary>>,
            List = urusai_db:get(MucK),
            case lists:filter(fun(X) -> X#muc_member.nick =:= Nick end, List) of
                []       -> [];
                [Result] -> Result#muc_member.jid
            end
    end.

%% ----------------------------------------
%% Private messages actions
%% ----------------------------------------

make_private_packet(From, To, Body) ->
    BodyX = try
        exmpp_message:chat(Body)
    catch _:_ ->
        exmpp_message:chat(<<"Malformed packet body, possible error in some plugin output.">>)
    end,
    exmpp_xml:set_attribute(
        exmpp_xml:set_attribute(
            BodyX, <<"from">>, From), <<"to">>, To).

%% Send alert 
alert(Session, Msg) ->
    lager:error("Alert: ~s", [Msg]),
    Me = current_jid(),
    [ send_packet(Session, make_private_packet(Me, O, Msg)) || O <- urusai_db:get(<<"owners">>) ].

%% ----------------------------------------
%% HTTP API handlers
%% ----------------------------------------

%% Handler for HTTP API messages
api_message(Session, Target, Body) ->
    case make_api_packet(Target, Body, target_type(Target)) of
        not_joined ->
            {error, not_joined};
        Packet ->
            send_packet(Session, Packet),
            {ok, sent}
    end.

%% Handler for HTTP API plugin requests
api_plugin(Session, Target, Body) ->
    {Joined, Type} = target_type(Target),
    api_plugin(Session, Target, Body, Type, Joined).

api_plugin(_Session, _Target, _Body, _Type, false) ->
    {error, not_joined};
api_plugin(Session, Target, Body, Type, true) ->
    case urusai_plugin:match(Type, Target, [], [Body]) of
        [none] ->
            {error, no_appropriate_plugins};
        Replies ->
            [send_packet(Session, make_api_packet(Target, E, {true, Type})) || E <- Replies],
            {ok, sent}
    end.

%% Create packet for API messages
make_api_packet(_Target, _Body, {false, _}) ->
    not_joined;
make_api_packet(Target, Body, {_, private}) ->
    make_private_packet(current_jid(), Target, Body);
make_api_packet(Target, Body, {_, mucmessage}) ->
    make_muc_packet(current_jid(), Target, Body).

%% Get target type from JID
target_type(Target) ->
    case {binary:match(Target, <<"@conference">>), binary:split(Target, <<"/">>)} of
        {nomatch, _}  -> {true, private};
        {_, [Target]} -> {is_joined(Target), mucmessage};
        _             -> {is_joined(Target), private}
    end.

%% ----------------------------------------
%% Other
%% ----------------------------------------

%% Get my current JID
current_jid() ->
    exmpp_jid:bare_to_binary(urusai_db:get(<<"current_jid">>)).

%% Is JID in owners list?
is_owner(Jid, false) ->
    Bare = exmpp_jid:bare_to_list(exmpp_jid:bare(exmpp_jid:parse(Jid))),
    lists:member(Bare, urusai_db:get(<<"owners">>));
%% Is JID MUC's owner?
is_owner(Jid, true) ->
    [Muc, User] = binary:split(Jid, <<"/">>),
    UserList = urusai_db:get(<<"muc_users_", Muc/binary>>),
    [] =/= lists:filter(fun(X) ->
            X#muc_member.nick =:= User andalso X#muc_member.affiliation =:= <<"owner">>
        end, UserList).

%% Am I joined to this MUC?
is_joined(Jid) ->
    Muc = exmpp_jid:bare_to_binary(exmpp_jid:bare(exmpp_jid:parse(Jid))),
    MUCs = urusai_db:get(<<"autojoin">>),
    lists:member(Muc, MUCs).

%% Update status message trigger
status_message(Session, Msg) ->
    urusai_db:set(<<"status">>, Msg),
    update_status(Session),
    {ok, <<"Status message updated.">>}.

%% Update status message
update_status(Session) ->
    Msg = urusai_db:get(<<"status">>),
    send_packet(Session, exmpp_presence:set_status(exmpp_presence:available(), Msg)).
