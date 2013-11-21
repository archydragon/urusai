%%% Database backend using DETS as KV-storage.
%%%
-module (urusai_db_dets).

-export ([run/1, set/3, get/2, stop/1]).

%% Start work with backend and return PID or other identifier used for further interaction
run(Config) ->
    {ok, dets_db} = dets:open_file(dets_db,
        [{file, proplists:get_value(file, Config)}, {type, set}]),
    dets_db.

%% Put value to database
set(Ref, Key, Value) ->
    case dets:insert(Ref, {Key, Value}) of
        true -> ok;
        _    -> error
    end.

%% Get value from database
get(Ref, Key) ->
    case dets:lookup(Ref, Key) of
        [{Key, Value}] -> Value;
        _ -> []
    end.

%% Required actions before DB interface process is stopped
stop(Ref) ->
    dets:close(Ref).
