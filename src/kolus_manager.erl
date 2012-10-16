-module(kolus_manager).
-behaviour(gen_server).

-include("kolus_internal.hrl").
-include("kolus.hrl").

-type kolus_socket() :: {reference(), port()}.

-record(state, {ip :: inet:ip_address(),
		port :: inet:port_number(),
		limit :: pos_integer(),
		active :: pos_integer(),
		idle :: pos_integer(),
		sockets :: [kolus_socket()]|[],
		managers_tid :: ets:tid()
	       }).

% API
-export([start_link/3]).

%% Callbacks
-export([init/1, handle_cast/2, handle_call/3,
	 handle_info/2, terminate/2, code_change/3]).

start_link(Tid, Ip, Port) ->
    gen_server:start_link(?MODULE, [Tid, Ip, Port]).

init([Tid, Ip, Port]) ->
    Limit = kolus_helper:get_env(socket_limit),
    ets:insert(Tid, {Ip, Port}, #backend{active=0,
					 idle=0,
					 limit=Limit,
					 pid=self()}),
    {ok, #state{ip=Ip,
		port=Port,
		active=0,
		idle=0,
		sockets=[],
		limit=kolus_helpers:get_env(socket_limit),
		managers_tid=Tid
	       }, kolus_helper:get_env(socket_timeout)}.

handle_cast({return, CallerMonitorRef, Socket}, #state{sockets=Sockets,
						       active=Active,
						       idle=Idle}=State) ->
    case lists:keyfind(CallerMonitorRef, 1, Sockets) of
	{CallerMonitorRef, Socket} ->
	    erlang:demonitor(process, CallerMonitorRef),
	    SocketTimerRef = erlang:start_timer(kolus_helper:get_env(socket_timeout), self(),
						timeout),
	    {noreply, State#state{active=Active-1,
				  idle=Idle+1,
				  sockets=Sockets++[{SocketTimerRef, Socket}]}};
	_ ->
	    % Someone returned a socket I don't own..
	    {noreply, State}
    end;
	
handle_cast(_Msg, State) ->
    {noreply, State}.

% Check if you can create a connection to this backend
% I'll assume the socket will be created, will add to active and start
% monitoring the client. Also return that monitoring reference for
% checkin time.
handle_call(can_create, _From, #state{active=Active,
				      idle=Idle,
				      limit=Limit}=State) when Active+Idle < Limit ->
    {reply, true, State};
handle_call(can_create, _From, State) ->
    {reply, false, State};
% The caller got permission to create a socket, sending it plus its pid
% for monitoring
handle_call({created, Socket, Caller}, _From, #state{sockets=Sockets,active=Active}=State) ->
    CallerMonitorRef = erlang:monitor(process, Caller),
    {reply, {ok, CallerMonitorRef}, State#state{sockets=Sockets ++ [{CallerMonitorRef, Socket}],
						active=Active+1}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_info(timeout, #state{sockets=[]}=State) ->
    {stop, normal, State};
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    Reason.

code_change(_, State, _) ->
    {ok, State}.
