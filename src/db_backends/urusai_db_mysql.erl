%%% Database backend using MySQL as KV-storage.
%%%
-module (urusai_db_mysql).

-export ([run/1, set/3, get/2, stop/1]).

-record (result_packet, {seq_num, field_list, rows, extra}).

%% Start work with backend and return PID or other identifier used for further interaction
run(Config) ->
    application:ensure_started(crypto),
    application:start(emysql),
    Pool = proplists:get_value(user, Config, mysql_pool),
    emysql:add_pool(
        Pool,
        1,
        proplists:get_value(user, Config),
        proplists:get_value(password, Config),
        proplists:get_value(host, Config, "localhost"),
        proplists:get_value(port, Config, 3306),
        proplists:get_value(database, Config),
        utf8),
    Table = list_to_binary(proplists:get_value(table, Config)),
    emysql:execute(Pool, <<"CREATE TABLE IF NOT EXISTS `", Table/binary,
        "` (`key` VARCHAR(256), `value` BLOB, PRIMARY KEY (`key`)) ENGINE=InnoDB">>),
    {Pool, Table}.

%% Put value to database
set({Pool, Table}, Key, Value) ->
    Result = emysql:execute(Pool, <<"REPLACE INTO `", Table/binary, "` SET `key`='", Key/binary,
        "', `value`='", Value/binary, "'">>),
    case Result of
        {ok_packet, _, _, _, _, _, _} -> ok;
        _                             -> error
    end.

%% Get value from database
get({Pool, Table}, Key) ->
    Result = emysql:execute(Pool, <<"SELECT `value` FROM `", Table/binary, "` WHERE `key`='", Key/binary, "'">>),
    case Result of
        _ when Result#result_packet.seq_num == 5 ->
            [[Value]] = Result#result_packet.rows, Value;
        _ ->
            <<131,106>>
    end.

%% Required actions before DB interface process is stopped
stop(_) ->
    application:stop(emysql).
