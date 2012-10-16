-module(kolus_helper).

-export([get_key_or_default/3]).

get_key_or_default(_, [], Default) ->
    Default;
get_key_or_default(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
	{Key, Val} ->
	    Val;
	_ ->
	    Default
    end.
