%%% Database backend using Redis as KV-storage.
%%%
-module (urusai_db_redis).

-export ([run/1, set/3, get/2, stop/1]).

%% Start work with backend and return PID or other identifier used for further interaction
run(Config) ->
    Hash = proplists:get_value(hash_name, Config),
    RedisConfig = proplists:delete(hash_name, Config),
    {ok, Pid} = eredis:start_link(RedisConfig),
    {Pid, Hash}.

%% Put value to database
set({Ref, Hash}, Key, Value) ->
    case eredis:q(Ref, ["HSET", Hash, Key, Value]) of
        {ok, _} -> ok;
        _       -> error
    end.

%% Get value from database
get({Ref, Hash}, Key) ->
    case eredis:q(Ref, ["HGET", Hash, Key]) of
        {ok, undefined} -> <<131,106>>;
        {ok, Result}    -> Result;
        _               -> <<131,106>>
    end.

%% Required actions before DB interface process is stopped
stop({Ref, _}) ->
    eredis:stop(Ref).
