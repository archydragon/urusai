%%% Interface for getting configuration values.
%%%
-module (urusai_config).

-behaviour (gen_server).

-define (SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([start_link/0, get/2]).

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

get(Group, Key) ->
    gen_server:call(?MODULE, {get, Group, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, state}.

handle_call({get, Group, Key}, _From, State) ->
    Reply = case application:get_env(urusai, Group) of
        undefined   -> {error, nogroup, Group}; % Don't delete groups from configuration!
        {ok, Found} -> proplists:get_value(Key, Found, default(Group, Key))
    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

default(auth, ssl)           -> false;
default(auth, port)          -> 5222;
default(muc, default_nick)   -> "Urusai!";
default(http, allow_private) -> false;
default(_, _)                -> [].
