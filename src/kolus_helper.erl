-module(kolus_helper).

-export([get_env/1]).

get_env(Key) ->
    {ok, Val} = application:get_env(Key),
    Val.
