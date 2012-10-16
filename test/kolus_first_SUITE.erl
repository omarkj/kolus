-module(kolus_first_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).

-export([init_per_suite/1,
	 end_per_suite/1]).

-export([no_managers/1]).

all() ->
    [no_managers].

% Setup & teardown
init_per_suite(Config) ->
    ok = start_application(kolus),
    % Start a server to connect to
    Port = start_server(),
    [{open_port, Port}|Config].

end_per_suite(Config) ->
    ok = application:stop(kolus),
    Config.

% No managers available
no_managers(Config) ->
    Port = ?config(open_port, Config),
    Backends = [{{127,0,0,1}, Port},
		{{127,0,0,1}, 1002}],
    [] = kolus:status(Backends),
    Backend = hd(Backends),
    Socket = kolus:connect(<<"test">>, Backend),
    case erlang:port_info(Socket) of
	A when is_list(A) ->
	    ok
    end,
    Config.

% Internal
start_application(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {not_started, App0}} ->
	    start_application(App0),
	    start_application(App)
    end.

start_server() ->
    {ok, Socket} = gen_tcp:listen(0, [binary,{active,false}]),
    {ok, PortNo} = inet:port(Socket),
    PortNo.
