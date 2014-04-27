%%% XMPP client. Need I say more?
%%%
-module (urusai_xmpp).

-behaviour (gen_server).

-define (SERVER, ?MODULE).
-define (CMD, urusai_xmpp_cmd).
-define (CMD_MUC, urusai_xmpp_cmd_muc).

-include_lib ("deps/exmpp/include/exmpp.hrl").
-include_lib ("deps/exmpp/include/exmpp_client.hrl").

-record (state, {
    session,
    queue,
    loop_timer = []
}).

-record (muc_member, {
    nick = <<>>,
    jid = <<>>,
    affiliation = <<>>,
    role = <<>>
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([start_link/0, parse_packet/2, version/0]).

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
    {ok, #state{session = connect(), queue = queue:new()}}.

handle_call({muc_join, Muc, Params}, _From, State) ->
    {reply, muc_join(Muc, Params, []), State};
handle_call({muc_join_protected, Muc, Params}, _From, State) ->
    {reply, muc_join_protected(Muc, Params), State};
handle_call({muc_leave, Muc}, _From, State) ->
    {reply, muc_leave(Muc), State};
handle_call({muc_nick, Muc, Nick}, _From, State) ->
    {reply, muc_nick(Muc, Nick), State};
handle_call({muc_kick, Muc, Nick}, _From, State) ->
    {reply, muc_kick(Muc, Nick, is_moderator(Muc)), State};
handle_call({muc_ban, Muc, Jid}, _From, State) ->
    {reply, muc_ban(Muc, Jid, is_admin(Muc)), State};
handle_call({status, Message}, _From, State) ->
    {reply, status_message(Message), State};
handle_call({api_message, Target, Body}, _From, State) ->
    {reply, api_message(Target, Body), State};
handle_call({api_plugin, Target, Body}, _From, State) ->
    {reply, api_plugin(Target, Body), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({muc_leave, Muc}, State) ->
    timer:sleep(200),
    muc_leave(Muc),
    {noreply, State};
handle_cast({send_packet, Packet}, #state{queue = Q} = State) ->
    NewState = State#state{queue = queue:in(Packet, Q)},
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, #state{queue = Q}) ->
    {noreply, #state{session = connect(), queue = Q}};
handle_info(after_connected, State) ->
    update_status(),
    muc_autojoin(),
    {noreply, State};
handle_info(queue_loop, State) ->
    catch erlang:cancel_timer(State#state.loop_timer),
    {Item, NewQueue} = queue:out(State#state.queue),
    case Item of
        empty      -> ok;
        {value, V} -> send_packet(State#state.session, V)
    end,
    NewState = State#state{
        queue = NewQueue,
        loop_timer = erlang:send_after(1000, ?MODULE, queue_loop)},
    {noreply, NewState};
handle_info(queue_stop_timer, #state{loop_timer = LoopTimer} = State) ->
    catch erlang:cancel_timer(LoopTimer),
    {noreply, State};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(restart, State) ->
    init:restart(),
    {noreply, State};
handle_info(die, State) ->
    init:stop(),
    {noreply, State};
handle_info(Packet, State) ->
    spawn_link(?MODULE, parse_packet, [State#state.session, Packet]),
    {noreply, State}.

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

    Session = exmpp_session:start(),

    JID = exmpp_jid:make(User, Server, Resource),
    lager:info("Trying to connect as ~s@~s to the server ~s:~B", [User, Server, ConnServer, Port]),
    urusai_db:set(<<"current_jid">>, JID), % required for alerts
    urusai_db:set(<<"started">>, calendar:now_to_local_time(now())),
    exmpp_session:auth_basic_digest(Session, JID, Password),
    ConnectMethod = case urusai_config:get(auth, ssl) of
        true  -> connect_SSL;
        false -> connect_TCP
    end,
    try
        ?MODULE ! queue_stop_timer,
        {ok, _Stream} = exmpp_session:ConnectMethod(Session, ConnServer, Port),
        lager:info("Connected."),
        exmpp_session:login(Session),
        lager:info("Logged in."),
        erlang:monitor(process, Session),
        erlang:send_after(1000, ?MODULE, after_connected),
        erlang:send_after(2000, ?MODULE, queue_loop)
    catch 
        _:{timeout, {gen_fsm, _, [Pid, _, _]}} = Err ->
            lager:error("Connection failed: ~p", [Err]),
            catch erlang:exit(Pid, kill),
            connect();
        _:{socket_error, _} = Err ->
            lager:error("Connection failed: ~p", [Err]),
            exmpp_session:stop(Session),
            timer:sleep(5000),
            connect();
        _:{auth_error, _} = Err ->
            lager:info("Login error: ~p!", [Err]),
            init:stop();
        _:Err ->
            lager:error("Error: ~p", [Err]),
            exmpp_session:stop(Session),
            connect()
    end,
    Session.

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
            handle_muc_message(Raw);
        % Private message
        #received_packet{packet_type=message,
                                  raw_packet=Raw,
                                  type_attr=Type} when Type =/= "error" ->
            handle_private(Raw);
        % IQ message
        #received_packet{packet_type=iq,
                                  raw_packet=Raw,
                                  type_attr=_Type} ->
            handle_iq(Raw);
        % Presence
        Record when Record#received_packet.packet_type == 'presence' ->
            handle_presence(Record, Record#received_packet.raw_packet);
        _Other ->
            lager:error("Unknown packet received: ~p", [Packet])
    end.

%% MUC message handler
handle_muc_message(Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    [Muc | User] = binary:split(From, <<"/">>),
    handle_muc_message(Packet, From, urusai_db:get(<<"muc_nick_", Muc/binary>>) =:= User).

handle_muc_message(_Packet, _From, true) ->
    ok;
handle_muc_message(Packet, From, false) ->
    Me = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    Command = exmpp_message:get_body(Packet),
    Matched = urusai_plugin:match(mucmessage, From, get_real_jid(From), [Command]),
    case lists:filter(fun(X) -> X =/= none end, Matched) of
        []    -> ok;
        Other -> [ gen_server:cast(?MODULE, {send_packet, make_muc_packet(Me, From, E)})
            || E <- muc_plugin_result(From, Other) ]
    end.

%% Private message handler
handle_private(Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    Me = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    IsMucUser = binary:match(From, <<"@conference.">>) =/= nomatch,
    handle_private(Packet, is_owner(From, IsMucUser), IsMucUser, From, Me).

%% Parse private message if it has been sent by MUC owner
%% TODO: optimize ownership code
handle_private(Packet, true, true, Target, Me) ->
    Command = exmpp_message:get_body(Packet),
    [Muc | _] = binary:split(Target, <<"/">>),
    [C | P] = binary:split(Command, <<" ">>),
    Body = case ?CMD_MUC:cmd(Muc, C, P) of
        {ok, Reply} -> Reply;
        error       -> <<"Bad command.">>
    end,
    gen_server:cast(?MODULE, {send_packet, make_private_packet(Me, Target, Body)});
%% Parse private message if it has been sent by bot owner
handle_private(Packet, true, false, Target, Me) ->
    Command = exmpp_message:get_body(Packet),
    [C | P] = binary:split(Command, <<" ">>),
    % In case owner want to execute some plugin command, he should send it with `exec ` prefix
    PBody = case ?CMD:cmd(C, P) of
        {ok, Reply} -> Reply;
        error       -> <<"Bad internal command.">>
    end,
    gen_server:cast(?MODULE, {send_packet, make_private_packet(Me, Target, PBody)});
%% Or if it has been sent by somebody else
handle_private(Packet, false, _, Target, Me) ->
    Command = exmpp_message:get_body(Packet),
    case urusai_plugin:match(private, Target, <<>>, [Command]) of
        [none] -> 
            gen_server:cast(?MODULE, {send_packet, make_private_packet(Me, Target, <<"No such command.">>)});
        Replies ->
            [gen_server:cast(?MODULE, {send_packet, make_private_packet(Me, Target, E)}) || E <- Replies]
    end.

%% IQ handler
handle_iq(Packet) ->
    % TODO: implement %)
    Type = exmpp_iq:get_type(Packet),
    case Type of
        get ->        
            Request = exmpp_iq:get_request(Packet),
            case exmpp_xml:get_ns_as_atom(Request) of
                'jabber:iq:version' ->
                    iq_client_version(Packet);
                'urn:xmpp:time' ->
                    iq_time(Packet);
                _Else ->
                    ok
            end;
        _ ->
            ok
    end,
    ok.

%% Presence handler
handle_presence(Packet, _Presence) ->
    case exmpp_jid:make({Conf, Serv, Nick} = Packet#received_packet.from) of
        JID ->
            C = <<Conf/binary, "@", Serv/binary>>,
            case _Type = Packet#received_packet.type_attr of
                "available" ->
                    muc_userjoined(C, Nick, Packet#received_packet.raw_packet);
                "unavailable" ->
                    muc_userleft(C, Nick, Packet#received_packet.raw_packet);
                "subscribe" ->
                    presence_subscribed(JID),
                    presence_subscribe(JID);
                "subscribed" ->
                    presence_subscribed(JID),
                    presence_subscribe(JID);
                "error" ->
                    M = <<Conf/binary, "@", Serv/binary>>,
                    urusai_db:set(<<"autojoin">>, lists:delete(M, urusai_db:get(<<"autojoin">>))),
                    ok
            end
    end.

%% ----------------------------------------
%% Presence actions
%% ----------------------------------------

presence_subscribed(Recipient) ->
    lager:info("Exchanging subscriptions with ~s", [Recipient]),
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    gen_server:cast(?MODULE, {send_packet, Presence}).

presence_subscribe(Recipient) ->
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    gen_server:cast(?MODULE, {send_packet, Presence}).

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

muc_join(Muc, Params, PacketBody) ->
    OldNick = urusai_db:get(<<"muc_nick_", Muc/binary>>),
    Nick = case Params of
        [] when OldNick =:= [] -> list_to_binary(urusai_config:get(muc, default_nick));
        []                     -> OldNick;
        _                      -> list_to_binary(Params)
    end,
    Presence = muc_join_packet(Muc, Nick, PacketBody),
    gen_server:cast(?MODULE, {send_packet, Presence}),
    lager:info("Joining MUC ~s as ~s", [Muc, Nick]),
    urusai_db:set(<<"autojoin">>, lists:usort(lists:append(urusai_db:get(<<"autojoin">>), [Muc]))),
    urusai_db:set(<<"muc_nick_", Muc/binary>>, Nick),
    urusai_db:set(<<"muc_password_", Muc/binary>>, PacketBody),
    urusai_db:set(<<"muc_users_", Muc/binary>>, []),
    {ok, <<"Presence sent.">>}.

muc_join_protected(_Muc, []) ->
    {ok, <<"Please specify the password for joining password protected MUC.">>};
muc_join_protected(Muc, [Password]) ->
    P = exmpp_xml:set_children(exmpp_xml:element('http://jabber.org/protocol/muc', x),
        [exmpp_xml:set_cdata(exmpp_xml:element(password), Password)]),
    muc_join(Muc, [], [P]).

muc_join_packet(Muc, Nick, Children) ->
    RP = exmpp_xml:set_attribute(
        exmpp_presence:set_status(exmpp_presence:available(), urusai_db:get(<<"status">>)),
        <<"to">>, <<Muc/binary, "/", Nick/binary>>),
    exmpp_xml:set_children(RP, Children).

muc_leave(Muc) ->
    RP = exmpp_presence:set_status(exmpp_presence:unavailable(), "kthxbye"),
    Presence = exmpp_xml:set_attribute(RP, <<"to">>, <<Muc/binary>>),
    gen_server:cast(?MODULE, {send_packet, Presence}),
    lager:info("Leaving MUC ~s", [Muc]),
    urusai_db:set(<<"autojoin">>, lists:delete(Muc, urusai_db:get(<<"autojoin">>))),
    {ok, <<"Presence sent.">>}.

muc_nick(Muc, [Nick]) ->
    RP = exmpp_presence:set_status(exmpp_presence:available(), urusai_db:get(<<"status">>)),
    Presence = exmpp_xml:set_attribute(RP, <<"to">>, <<Muc/binary, "/", Nick/binary>>),
    gen_server:cast(?MODULE, {send_packet, Presence}),
    lager:info("Changed nick at MUC ~s to ~s", [Muc, Nick]),
    urusai_db:set(<<"muc_nick_", Muc/binary>>, Nick),
    {ok, <<"Presence sent.">>}.

muc_kick(_Muc, _Nick, false) ->
    {ok, <<"I'm not a moderator there.">>};
muc_kick(Muc, [Nick], true) ->
    BareMuc = exmpp_jid:parse(Muc),
    gen_server:cast(?MODULE, {send_packet, exmpp_client_muc:kick(BareMuc, Nick)}),
    {ok, <<"Kicked.">>}.

muc_ban(_Muc, _Jid, false) ->
    {ok, <<"I'm not an administrator there.">>};
muc_ban(Muc, [Jid], true) ->
    BareMuc = exmpp_jid:parse(Muc),
    BareJid = exmpp_jid:parse(Jid),
    gen_server:cast(?MODULE, {send_packet, exmpp_client_muc:ban(BareMuc, BareJid)}),
    {ok, <<"Banned.">>}.

muc_autojoin() ->
    [ muc_join(A, [], urusai_db:get(<<"muc_password_", A/binary>>)) || A <- urusai_db:get(<<"autojoin">>) ].

muc_userjoined(Conf, Nick, Raw) ->
    muc_userjoined_save(Conf, Nick, muc_getdata(Conf, Nick, <<"join">>, Raw)).

muc_userjoined_save(_Conf, _Nick, ok) ->
    ok;
muc_userjoined_save(Conf, Nick, {_Presence, Jid, Affiliation, Role, _NewNick} = Data) ->
    User = #muc_member{
        nick = Nick,
        jid = Jid,
        affiliation = Affiliation,
        role = Role
    },
    muc_presence_match(<<Conf/binary, "/", Nick/binary>>, Jid, Data),
    MucK = <<"muc_users_", Conf/binary>>,
    urusai_db:set(MucK, lists:append(lists:filter(fun(X) -> X#muc_member.nick =/= Nick end, urusai_db:get(MucK)), [User])),
    ok.

muc_userleft(Conf, Nick, Raw) ->
    muc_userleft_save(Conf, Nick, muc_getdata(Conf, Nick, <<"leave">>, Raw)).

muc_userleft_save(_Conf, _Nick, ok) ->
    ok;
muc_userleft_save(Conf, Nick, {_Presence, Jid, _Affiliation, _Role, _NewNick} = Data) ->
    muc_presence_match(<<Conf/binary, "/", Nick/binary>>, Jid, Data),
    MucK = <<"muc_users_", Conf/binary>>,
    urusai_db:set(MucK, lists:filter(fun(X) -> X#muc_member.nick =/= Nick end, urusai_db:get(MucK))),
    ok.

muc_getdata(Conf, Nick, Presence, Raw) ->
    MyNick = urusai_db:get(<<"muc_nick_", Conf/binary>>),
    Type = case muc_statuscode(Raw) of
        307 when Nick =:= MyNick -> muc_autojoin(), <<"kick">>;
        307                      -> <<"kick">>;
        301                      -> <<"ban">>;
        _                        -> Presence
    end,
    case exmpp_xml:get_element(exmpp_xml:get_element(Raw, 'http://jabber.org/protocol/muc#user', x), item) of
        undefined ->
            ok;
        Data ->
            list_to_tuple([Type] ++ [ exmpp_xml:get_attribute(Data, A, <<>>)
                || A <- [<<"jid">>, <<"affiliation">>, <<"role">>, <<"nick">>] ])
    end.

muc_statuscode(Raw) ->
    case exmpp_xml:get_attribute(exmpp_xml:get_element(
            exmpp_xml:get_element(Raw, 'http://jabber.org/protocol/muc#user', x), status), <<"code">>, <<>>) of
        <<>> -> 0;
        B    -> binary_to_integer(B)
    end.

muc_presence_match(From, Jid, Data) ->
    Matched = urusai_plugin:match(mucpresence, From, Jid, [Data]),
    case lists:filter(fun(X) -> X =/= none end, Matched) of
        []    -> ok;
        Other -> [ gen_server:cast(?MODULE, {send_packet, make_muc_packet(current_jid(), From, E)}) || E <- Other ]
    end.

muc_plugin_result(From, Returned) ->
    [ case R of
        _ when is_binary(R) ->
            R;
        {<<"mucmessage">>, Body} ->
            Body;
        {<<"privmessage">>, Target, Body} ->
            gen_server:cast(?MODULE, {send_packet, make_private_packet(current_jid(), Target, Body)}),
            <<"">>;
        {<<"kick">>, Target} ->
            [Muc, _] = binary:split(From, <<"/">>),
            gen_server:call(?MODULE, {muc_kick, Muc, [Target]}),
            <<"">>;
        {<<"ban">>, Target} ->
            [Muc, _] = binary:split(From, <<"/">>),
            gen_server:call(?MODULE, {muc_ban, Muc, [Target]}),
            <<"">>;
        _Otherwise ->
            <<"Some plugin returned bad data.">>
    end || R <- Returned ].

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
alert(Msg) ->
    lager:error("Alert: ~s", [Msg]),
    Me = current_jid(),
    [ gen_server:cast(?MODULE, {send_packet, make_private_packet(Me, O, Msg)}) || O <- urusai_db:get(<<"owners">>) ].

%% ----------------------------------------
%% IQ actions
%% ----------------------------------------

%% IQ result generator
make_iq_result(NS, Element, Children) ->
    XMLChildren = [ exmpp_xml:set_cdata(exmpp_xml:element(NS, E, [], []), V) || {E, V} <- Children ],
    exmpp_xml:set_children(exmpp_xml:element(NS, Element), XMLChildren).

%% IQ response — client version
iq_client_version(Request) ->
    NS = 'jabber:iq:version',
    Result = make_iq_result(NS, 'query', [{"name", <<"Urusai">>}, {"version", version()}]),
    gen_server:cast(?MODULE, {send_packet, exmpp_iq:result(Request, Result)}).

%% IQ response — time and timezone
iq_time(Request) ->
    NS = 'urn:xmpp:time',
    RawUTC = calendar:universal_time(),
    RawLocal = calendar:local_time(),
    {{Y, M, D}, {Hh, Mm, Ss}} = RawUTC,
    TimeUTC = io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ", [Y, M, D, Hh, Mm, Ss]),
    {Sign, DH, DM} = case calendar:time_difference(RawUTC, RawLocal) of
        {-1, _} -> {_, {DifH, DifM, _}} = calendar:time_difference(RawLocal, RawUTC), {"-", DifH, DifM};
        Else    -> {_, {DifH, DifM, _}} = Else, {"+", DifH, DifM}
    end,
    TZ = io_lib:format("~s~2..0w:~2..0w", [Sign, DH, DM]),
    Result = make_iq_result(NS, 'time', [{"tzo", TZ}, {"utc", TimeUTC}]),
    gen_server:cast(?MODULE, {send_packet, exmpp_iq:result(Request, Result)}).

%% ----------------------------------------
%% HTTP API handlers
%% ----------------------------------------

%% Handler for HTTP API messages
api_message(Target, Body) ->
    case make_api_packet(Target, Body, target_type(Target)) of
        not_joined ->
            {error, not_joined};
        Packet ->
            gen_server:cast(?MODULE, {send_packet, Packet}),
            {ok, sent}
    end.

%% Handler for HTTP API plugin requests
api_plugin(Target, Body) ->
    {Joined, Type} = target_type(Target),
    api_plugin(Target, Body, Type, Joined).

api_plugin(_Target, _Body, _Type, false) ->
    {error, not_joined};
api_plugin(Target, Body, Type, true) ->
    Matched = urusai_plugin:match(Type, Target, [], [Body]),
    case lists:filter(fun(X) -> X =/= none end, Matched) of
        [] ->
            {error, no_appropriate_plugins};
        Replies ->
            [gen_server:cast(?MODULE, {send_packet, make_api_packet(Target, E, {true, Type})}) || E <- Replies],
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

%% Am I moderator of this MUC?
is_moderator(Muc) ->
    UserList = urusai_db:get(<<"muc_users_", Muc/binary>>),
    MyNick = urusai_db:get(<<"muc_nick_", Muc/binary>>),
    [] =/= lists:filter(fun(X) ->
            X#muc_member.nick =:= MyNick andalso X#muc_member.role =:= <<"moderator">>
        end, UserList).

%% Am I administrator of this MUC?
is_admin(Muc) ->
    UserList = urusai_db:get(<<"muc_users_", Muc/binary>>),
    MyNick = urusai_db:get(<<"muc_nick_", Muc/binary>>),
    [] =/= lists:filter(fun(X) ->
            A = [<<"admin">>, <<"owner">>],
            X#muc_member.nick =:= MyNick andalso lists:member(X#muc_member.affiliation, A)
        end, UserList).

%% Update status message trigger
status_message(Msg) ->
    urusai_db:set(<<"status">>, Msg),
    update_status(),
    {ok, <<"Status message updated.">>}.

%% Update status message
update_status() ->
    Msg = urusai_db:get(<<"status">>),
    gen_server:cast(?MODULE, {send_packet, exmpp_presence:set_status(exmpp_presence:available(), Msg)}).

%% Version info
-spec version() -> Version :: binary().
version() ->
    [{urusai, _, VersionString}] = lists:filter(fun({App, _, _}) -> App =:= urusai end,
        application:loaded_applications()),
    list_to_binary(VersionString).
