-module(kolus_helper).

-export([get_env/1,
	 get_key_or_default/3]).

get_env(Key) ->
    {ok, Val} = application:get_env(Key),
    Val.

get_key_or_default(_, [], Default) ->
    Default;
get_key_or_default(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
	{Key, Val} ->
	    Val;
	_ ->
	    Default
    end.
