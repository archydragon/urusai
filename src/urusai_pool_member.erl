%%% Pool member intarface. Spawns Python process and proceeds calls to it.
%%% The only interesting fuinction is handle_call({call, M, F, A}, _From, {Lang, Pid})
%%%
-module (urusai_pool_member).

-behaviour (gen_server).

-define (SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([start_link/0, start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    start_link({python, [{python_path, "include:plugins"}]}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Lang, Args}) ->
    {ok, Pid} = Lang:start_link(Args),
    {ok, {Lang, Pid}}.

handle_call({call, M, F, A}, _From, {Lang, Pid}) ->
    Reply = try
        Lang:call(Pid, M, F, A, [{timeout, 60000}])
    catch error:{Lang, Class, Arg, Stacktrace} ->
        lager:error("Error in plugin '~p' (~p: ~p)", [M, Class, Arg]),
        lager:error("Stack trace: ~p", [Stacktrace]),
        ReplyStr = io_lib:format("Internal error in '~s' plugin (~s: ~s).", [M, Class, Arg]),
        list_to_binary(ReplyStr)
    end,
    {reply, Reply, {Lang, Pid}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {Lang, Pid}) ->
    Lang:stop(Pid).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
