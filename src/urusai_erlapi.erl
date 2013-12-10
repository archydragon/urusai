%%% Some functions used by Python plugin API
%%% 
-module (urusai_erlapi).

-export ([
    available_plugins/0,
    available_plugins/1,
    db_get/1,
    db_set/2
]).

-spec available_plugins() -> [binary()].
available_plugins() ->
    [ a_to_bin(P) || P <- lists:delete(help, urusai_plugin:plugins(private))].

-spec available_plugins(Muc :: binary()) -> [binary()].
available_plugins(Muc) ->
    All = sets:from_list(urusai_plugin:plugins(mucmessage)),
    Enabled = sets:from_list(lists:delete(help, urusai_db:get(<<"muc_plugins_", Muc/binary>>))),
    [ a_to_bin(P) || P <- sets:to_list(sets:intersection(Enabled, All)) ].

-spec a_to_bin(Atom :: atom()) -> binary().
a_to_bin(Atom) ->
    list_to_binary(atom_to_list(Atom)).

-spec db_get(Key :: binary() | string()) -> any().
db_get(Key) ->
    urusai_db:get(Key).

-spec db_set(Key :: binary() | string(), Value :: any()) -> true.
db_set(Key, Value) ->
    urusai_db:set(Key, Value),
    true.
