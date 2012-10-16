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
    ok = application:set_env(kolus, socket_limit, 10),
    ok = application:set_env(kolus, socket_timeout, 10000),
    % Start a server to connect to
    {Port,Pid} = start_server(),
    % Set required configs
    [{open_port, Port},
     {server_pid, Pid}|Config].

end_per_suite(Config) ->
    ok = application:stop(kolus),
    Config.

% No managers available
no_managers(Config) ->
    Port = ?config(open_port, Config),
    Backends = [{{127,0,0,1}, Port},
		{{127,0,0,1}, 1002}],
    [] = kolus:status(Backends),
    {socket, KSocket} = kolus:connect(<<"test">>, hd(Backends)),
    case erlang:port_info(kolus:get_socket(KSocket)) of
	A when is_list(A) ->
	    ok
    end,
    % Check unused, should be 9
    [{{{127,0,0,1},Port},_Pid,[{idle,0},{unused,9}]}] = kolus:status(Backends),
    % Return socket
    ok = kolus:return(KSocket),
    Status = kolus:status(Backends),
    % Need to wait for the insert to finish, do it somewhere else?
    timer:sleep(1),
    [{{{127,0,0,1},Port},_Pid,[{idle,1},{unused,9}]}] = kolus:status(Backends),
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
    {ok, Socket} = gen_tcp:listen(0, [{active,false}]),
    
    {ok, PortNo} = inet:port(Socket),
    Server = spawn(fun() -> listener() end),
    gen_tcp:controlling_process(Socket, Server),
    Server ! {socket, Socket},
    {PortNo, Server}.

listen(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	_ ->
	    listen(Socket)
    end.

listener() ->
    receive
	{socket, Socket} ->
	    {ok, Socket0} = gen_tcp:accept(Socket),
	    listen(Socket0)
    end.
