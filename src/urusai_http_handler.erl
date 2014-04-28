%%% API HTTP handler
%%% 
-module (urusai_http_handler).

-behaviour (cowboy_http_handler).

-export ([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
    {ok, Req, undefined_state}.
 
handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {Code, Message} = reply(Req, Method),
    {ok, Req2} = cowboy_req:reply(Code, [
        {<<"content-type">>, <<"application/json; charset=utf-8">>}
    ], Message, Req),
    {ok, Req2, State}.
 
terminate(_Reason, _Req, _State) ->
    ok.

%% Parse HTTP request
-spec reply(Req :: cowboy_req:req(), Method :: binary()) -> {Code :: integer(), Msg :: binary()}.
reply(Req, <<"POST">>) ->
    {ok, ReqBody, _Req} = cowboy_req:body(Req),
    lager:info("Received HTTP API request: ~s", [ReqBody]),
    case jsonx:decode(ReqBody, [{format, proplist}]) of
        {error, Error, Pos} ->
            {200, jsonx:encode([{result, error},
                {message, list_to_binary(io_lib:format("~s (at ~B)", [Error, Pos]))}])};
        Decoded ->
            [Type, Target, Body] =
                [ proplists:get_value(K, Decoded) || K <- [<<"type">>, <<"target">>, <<"body">>] ],
            {Result, Message} = call_xmpp(
                Type,
                urusai_config:get(http, allow_private)
                    or lists:member(Target, urusai_db:get(<<"muc_http_enabled">>)),
                Target,
                Body
            ),
            {200, jsonx:encode([{result, Result}, {message, Message}])}
    end;
reply(_Req, _) ->
    {405, <<>>}.

%% Validate decoded data and try to call XMPP or plugin API
-spec call_xmpp(Type :: binary(), Allow :: boolean(), Target :: binary(), Body :: binary()) -> {ok | error, binary()}.
call_xmpp(_Type, _Allow, undefined, _Body) ->
    {error, target_not_set};
call_xmpp(_Type, _Allow, _Target, undefined) ->
    {error, body_not_set};
call_xmpp(_Type, false, _Target, _Body) ->
    {error, not_allowed};
call_xmpp(<<"message">>, true, Target, Body) ->
    case unicode:characters_to_list(Body) of
        {error, _, _} -> {error, utf8_messages_only};
        _Else         -> gen_server:call(urusai_xmpp, {api_message, Target, Body})
    end;
call_xmpp(<<"plugin">>, true, Target, Body) ->
    gen_server:call(urusai_xmpp, {api_plugin, Target, Body});
call_xmpp(_Type, _Allow, _Target, _Body) ->
    {error, unknown_message_type}.
