%%% Plugin manager. Loads and indexes plugins, calls processes from pool for execution.
%%%
-module (urusai_plugin).

-behaviour (gen_server).

-define (SERVER, ?MODULE).

-define (pluginTypes, [private, mucmessage, mucpresence]).

-record (plugin, {
    trigger = <<>>,
    module = <<>>,
    method = <<>>
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([start_link/0]).

-export ([plugins/0, plugins/1, match/4, reload/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Reload plugin module and all Python processes in pool
reload() ->
    application:stop(pooler),
    application:start(pooler),
    gen_server:call(?MODULE, reload, 60000).

%% Run Python function execution
run(Type, _Module, _Function, _Args, false) when Type =:= mucmessage ->
    none;
run(_Type, Module, Function, Args, _) ->
    try
        call_pool_member(Module, Function, Args)
    catch exit:{timeout, _} ->
        list_to_binary(io_lib:format("Call to the method '~s' (module '~s') timed out.", [Function, Module]))
    end.

%% Get list of loaded plugins
plugins() ->
    gen_server:call(?MODULE, plugins).

plugins(Type) ->
    gen_server:call(?MODULE, {plugins, Type}).

%% Find matching triggers and execute appropriate Python functions
match(_Type, _From, _FromJID, [Value]) when Value =:= undefined ->
    [none];
match(Type, From, FromJID, [Value]) when is_tuple(Value) andalso Type =:= mucpresence ->
    [BaseJid | _] = binary:split(From, <<"/">>),
    Actions = ets:tab2list(Type),
    case Actions of
        [] -> [none];
        _  -> [ run(
                    Type,
                    C#plugin.module,
                    C#plugin.method,
                    [From, FromJID, Value],
                    lists:member(C#plugin.module, urusai_db:get(<<"muc_plugins_", BaseJid/binary>>))) 
              || C <- Actions ]
    end;
match(Type, From, FromJID, [Value]) ->
    [BaseJid | _] = binary:split(From, <<"/">>),
    Actions = ets:tab2list(Type),
    case [ A || #plugin{trigger = T} = A <- Actions, re:run(Value, T) =/= nomatch ] of
        []    -> [none];
        CanDo -> [ run(
                    Type,
                    C#plugin.module,
                    C#plugin.method,
                    [From, FromJID, Value],
                    lists:member(C#plugin.module, urusai_db:get(<<"muc_plugins_", BaseJid/binary>>))) 
                 || C <- CanDo ]
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    get_plugins(),
    {ok, []}.

handle_call(plugins, _From, State) ->
    Reply = [ {T, ets:tab2list(T)} || T <- ?pluginTypes ],
    {reply, Reply, State};
handle_call({plugins, Type}, _From, State) ->
    Reply = lists:usort([ P#plugin.module || P <- ets:tab2list(Type) ]),
    {reply, Reply, State};
handle_call(reload, _From, State) ->
    {reply, get_plugins(), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_plugin, Type, Plugin}, State) ->
    ets:insert(Type, Plugin),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% Run 'urusai_plugin.getPluginsFromAll()' function and format its output to useable ETS table
get_plugins() ->
    lager:info("Loading plugins"),
    RawPlugins = call_pool_member(urusai_plugin, 'getPluginsFromAll', []),
    % Clear ETS tables (required for proper plugins reload)
    lists:map(fun(T) ->
        case ets:info(T) of
            undefined -> ok;
            _         -> ets:delete(T)
        end,
        ets:new(T, [duplicate_bag, named_table])
    end, ?pluginTypes),
    Loaded = format_plugins(RawPlugins),
    Triggers = lists:sum(lists:flatten(Loaded)),
    ModFound = length(RawPlugins),
    ModUsed = length(lists:filter(fun(E) -> E =/= 0 end, Loaded)),
    lager:info("Found ~p modules, loaded ~p of them (~p triggers)", [ModFound, ModUsed, Triggers]),
    ok.

%% Format raw output from Python function to records
format_plugins(Raw) ->
    [ format_plugins(M, Body) || {M, Body} <- Raw ].

format_plugins(Module, {bad_triggers, C}) ->
    lager:error("Failed to load triggers from plugin class '~s' (module '~s')", [C, Module]),
    0;
format_plugins(Module, Raw) ->
    [ format_plugins(Module, C, Type, Body) || {C, Type, Body} <- Raw ].

format_plugins(Module, Class, _, notriggers) ->
    lager:error("Plugin \"~s\" (module '~s') has no triggers.", [Class, Module]),
    0;
format_plugins(Module, Class, Type, Raw) ->
    [ add_plugin(Type, #plugin{
        trigger = Regex,
        module = bin_to_atom(Module),
        method = bin_to_atom(<<"plugin", Class/binary, ".", "trigger", Method/binary>>)})
    || {Regex, Method} <- Raw ],
    length(Raw).

%% Insert parsed plugin trigger details to appropriate ETS table
add_plugin(Type, Plugin) ->
    gen_server:cast(?MODULE, {add_plugin, Type, Plugin}).

%% Call for work one member from pool
call_pool_member(M, F, A) ->
    P = take_pool_member(),
    Reply = gen_server:call(P, {call, M, F, A}, 60000),
    pooler:return_member(urusai_config:get(common, pool_name), P),
    Reply.

%% Try to get a member from the pool; if failed, wait 5 seconds and try again
take_pool_member() ->
    case pooler:take_member(urusai_config:get(common, pool_name)) of
        error_no_members -> timer:sleep(5000), take_pool_member();
        Else             -> Else
    end.

%% Helper to convert binaries to atom
bin_to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).
