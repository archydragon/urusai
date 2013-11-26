-module(urusai_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 10, 10}, [
        ?CHILD(urusai_config, worker),
        ?CHILD(urusai_db, worker),
        ?CHILD(urusai_plugin, worker),
        ?CHILD(urusai_xmpp, worker)
    ]} }.

