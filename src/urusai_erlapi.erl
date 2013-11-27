%%% Some functions used by Python plugin API
%%% 
-module (urusai_erlapi).

-export ([available_plugins/1]).

available_plugins(Muc) ->
    All = sets:from_list(urusai_plugin:plugins(mucmessage)),
    Enabled = sets:from_list(lists:delete(help, urusai_db:get(<<"muc_plugins_", Muc/binary>>))),
    [ a_to_bin(P) || P <- sets:to_list(sets:intersection(Enabled, All)) ].

a_to_bin(Atom) ->
    list_to_binary(atom_to_list(Atom)).