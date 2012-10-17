-module(kolus_first_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../src/kolus_internal.hrl").

-export([all/0]).

-export([init_per_suite/1,
	 end_per_suite/1]).

-export([no_managers/1,
	 get_idle/1,
	 return_unusable/1,
	 socket_timeout/1,
	 manager_timeout/1,
	 full_manager/1,
	 changed_ident/1,
	 fill_first/1
	]).

all() ->
    [no_managers
     ,get_idle
     ,return_unusable
     ,socket_timeout
     ,manager_timeout
     ,full_manager
     ,changed_ident
     ,fill_first
    ].

% Setup & teardown
init_per_suite(Config) ->
    ok = start_application(kolus),
    % Start a server to connect to
    {Port,Pid} = start_server(),
    % Set required configs
    Backends = [{{127,0,0,1}, Port},
		{{127,0,0,1}, 1002}],
    [{open_port, Port},
     {backends, Backends},
     {server_pid, Pid}|Config].

end_per_suite(Config) ->
    ok = application:stop(kolus),
    Config.

% No managers available
no_managers(Config) ->
    Port = ?config(open_port, Config),
    Backends = ?config(backends, Config),
    [] = kolus:status(Backends),
    {socket, KSocket} = kolus:connect(<<"test">>, hd(Backends)),
    case erlang:port_info(kolus:get_socket(KSocket)) of
	A when is_list(A) ->
	    ok
    end,
    % Check unused, should be 9
    [{{{127,0,0,1},Port},_Pid,{idle,0},{unused,9}}] = kolus:status(Backends),
    % Return socket
    ok = kolus:return(KSocket),
    % Need to wait for the insert to finish, do it somewhere else - this is needed
    % for the test, not in regular usage.
    timer:sleep(1),
    [{{{127,0,0,1},Port},_Pid,{idle,1},{unused,9}}] = kolus:status(Backends),
    {save_config, [{old_socket,KSocket}]}.

% Get the idle socket created in the test above
get_idle(Config) ->
    Backends = ?config(backends, Config),
    {_, SavedConfig} = ?config(saved_config, Config),
    OldSocket = proplists:get_value(old_socket, SavedConfig),
    [{{{127,0,0,1},Port},Pid,{idle,1},{unused,9}}] = kolus:status(Backends),
    {socket, KSocket} = kolus:connect(<<"test">>, Pid),
    true = kolus:get_socket(OldSocket) == kolus:get_socket(KSocket),
    timer:sleep(1),
    [{{{127,0,0,1},Port},_Pid,{idle,0},{unused,9}}] = kolus:status(Backends),
    ok = kolus:return(KSocket),
    timer:sleep(1),
    [{{{127,0,0,1},Port},_Pid,{idle,1},{unused,9}}] = kolus:status(Backends),
    Config.

% Return unusable socket
return_unusable(Config) ->
    Backends = ?config(backends, Config),
    [{{{127,0,0,1},Port},Pid,{idle,1},{unused,9}}] = kolus:status(Backends),
    {socket, KSocket} = kolus:connect(<<"test">>, Pid),
    gen_tcp:close(kolus:get_socket(KSocket)),
    ok = kolus:finished(KSocket),
    timer:sleep(1),
    [{{{127,0,0,1},Port},Pid,{idle,0},{unused,10}}] = kolus:status(Backends),
    Config.

% A socket times out that's being held by the manager
socket_timeout(Config) ->
    Backends = ?config(backends, Config),
    ok = application:set_env(kolus, socket_idle_timeout, 100),
    [{{{127,0,0,1},_Port},Pid,{idle,0},{unused,10}}] = kolus:status(Backends),
    {socket, KSocket} = kolus:connect(<<"test">>, Pid),
    ok = kolus:return(KSocket),
    timer:sleep(101),
    [{{{127,0,0,1},_Port},Pid,{idle,0},{unused,10}}] = kolus:status(Backends),
    ok = application:set_env(kolus, socket_idle_timeout, 5000),
    Config.

% A manager times out
manager_timeout(Config) ->
    Backends = ?config(backends, Config),
    ok = application:set_env(kolus, socket_idle_timeout, 100),
    ok = application:set_env(kolus, manager_idle_timeout, 100),
    % Start by shutting down the current manager
    [{{{127,0,0,1},_Port},Pid,{idle,0},{unused,10}}] = kolus:status(Backends),
    ok = kolus_manager:stop_sync(Pid),
    [] = kolus:status(Backends),
    {socket, KSocket} = kolus:connect(<<"test">>, hd(Backends)),
    MngrPid = kolus:get_manager(KSocket),
    % Wait here for the manager idle time, nothing should happen
    timer:sleep(101),
    true = erlang:is_process_alive(MngrPid),
    % Return the socket, after the socket timeout (100ms) + the manager
    % timeout (100ms) the manager will be history. A bit of grace period
    % is also nice.
    ok = kolus:return(KSocket),
    timer:sleep(250),
    false = erlang:is_process_alive(MngrPid),
    ok = application:set_env(kolus, socket_idle_timeout, 5000),
    ok = application:set_env(kolus, manager_idle_timeout, 5000),
    Config.

% Full manager
full_manager(Config) ->
    Backends = ?config(backends, Config),
    ok = application:set_env(kolus, endpoint_connection_limit, 2),
    {socket, KSocket} = kolus:connect(<<"test">>, hd(Backends)),
    {socket, KSocket1} = kolus:connect(<<"test">>, hd(Backends)),
    [{{{127,0,0,1},_Port},_Pid,{idle,0},{unused,0}}] = kolus:status(Backends),
    {error, rejected} = kolus:connect(<<"test">>, hd(Backends)),
    ok = kolus:return(KSocket),
    {socket, KSocket2} = kolus:connect(<<"test">>, hd(Backends)),
    ok = kolus:return(KSocket2),
    ok = kolus:return(KSocket1),
    ok = application:set_env(kolus, endpoint_connection_limit, 10),
    Config.

% Dealing with different incoming connection idents
changed_ident(Config) ->
    Backends = ?config(backends, Config),
    {socket, KSocket} = kolus:connect(<<"test">>, hd(Backends)),
    {error, rejected} = kolus:connect(<<"test0">>, hd(Backends)),
    {socket, KSocket1} = kolus:connect(<<"test">>, hd(Backends)),
    ok = kolus:return(KSocket),
    ok = kolus:return(KSocket1),
    Config.

% Very simple fill first algo
fill_first(Config) ->
    % Start by creating a few servers
    {Port0,_P1} = start_server(),
    {Port1,_P2} = start_server(),
    {Port2,_P3} = start_server(),
    L = {127,0,0,1},
    B0 = {L, Port0},
    B1 = {L, Port1},
    B2 = {L, Port2},
    Backends = [B0,B1,B2],
    % No backends exist
    [] = kolus:status(Backends),
    % To make this test slighty more annoying I'm going to connect
    % to a few sockets on all the backends. This distribution means
    % that B0 should be filled up first, then B1, then B2.
    {socket, _} = kolus:connect(<<"test">>, B1),
    {socket, _} = kolus:connect(<<"test">>, B1),
    {socket, _} = kolus:connect(<<"test">>, B2),
    {socket, _} = kolus:connect(<<"test">>, B0),
    {socket, _} = kolus:connect(<<"test">>, B0),
    {socket, _} = kolus:connect(<<"test">>, B0),
    % Start by checking the status of all the managers
    [{{{127,0,0,1},Port0},_,{idle,0},{unused,A}},
     {{{127,0,0,1},Port1},_,{idle,0},{unused,B}},
     {{{127,0,0,1},Port2},_,{idle,0},{unused,C}}] = kolus:status(Backends),
    % Fill up the sockets. This is going on on a single process and does take
    % a while.
    ok = fill_backend(Backends, A),
    [{{{127,0,0,1},Port0},_,{idle,0},{unused,0}},
     {{{127,0,0,1},Port1},_,{idle,0},{unused,B}},
     {{{127,0,0,1},Port2},_,{idle,0},{unused,C}}] = kolus:status(Backends),
    ok = fill_backend(Backends, B),
    [{{{127,0,0,1},Port0},_,{idle,0},{unused,0}},
     {{{127,0,0,1},Port1},_,{idle,0},{unused,0}},
     {{{127,0,0,1},Port2},_,{idle,0},{unused,C}}] = kolus:status(Backends),
    ok = fill_backend(Backends, C),
    [{{{127,0,0,1},Port0},_,{idle,0},{unused,0}},
     {{{127,0,0,1},Port1},_,{idle,0},{unused,0}},
     {{{127,0,0,1},Port2},_,{idle,0},{unused,0}}] = kolus:status(Backends),
    Config.

% Internal
fill_backend(_, 0) ->
    ok;
fill_backend(Backends, Runs) ->
    kolus:select(<<"test">>, Backends, fun ff_filter/1),
    fill_backend(Backends, Runs-1).

ff_filter(Backends) ->
    {Backend,_,_,_} = 
	hd(lists:sort(fun
			  ({_,_,{idle,A},{unused,X}},
			   {_,_,{idle,B},{unused,Y}}) ->
			      case A > B of
				  true ->
				      true;
				  false ->
				      case X of
					  0 ->
					      false;
					  _ ->
					      X =< Y
				      end
			      end
		      end, Backends)),
    Backend.

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
