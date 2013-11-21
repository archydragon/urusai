%%% Interface for general-purpose key-value database. May use any backend named
%%% e.g. 'urusai_db_dets' and properly configured.
%%% Look to `src/db_backends` directory for backend module examples.
%%%
-module (urusai_db).

-behaviour (gen_server).

-define (SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([start_link/0]).

-export ([set/2, get/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    L = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    generate_default(),
    L.

set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    B = urusai_config:get(database, backend),
    Module = list_to_atom("urusai_db_" ++ atom_to_list(B)),
    Config  = urusai_config:get(database, config),
    Session = Module:run(Config),
    {ok, {Module, Session}}.

handle_call({set, Key, Value}, _From, {Module, Session}) ->
    Reply = Module:set(Session, Key, Value),
    {reply, Reply, {Module, Session}};
handle_call({get, Key}, _From, {Module, Session}) ->
    Reply = Module:get(Session, Key),
    {reply, Reply, {Module, Session}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {Module, Session}) ->
    Module:stop(Session),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% Put to the database default
generate_default() ->
    [ generate_default(M) || M <- maybe_default(), urusai_db:get(M) =:= [] ].

generate_default(<<"owners">>) ->
    urusai_db:set(<<"owners">>, [urusai_config:get(common, owner)]);
generate_default(<<"status">>) ->
    urusai_db:set(<<"status">>, <<"Up and running">>);
generate_default(_NoSuchKey) ->
    error.

%% List of the values that should be set by default
maybe_default() ->
    [
        <<"owners">>,
        <<"status">>
    ].
