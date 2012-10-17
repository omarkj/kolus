-module(kolus_manager).
-behaviour(gen_server).

-include("kolus_internal.hrl").
-include("kolus.hrl").

-record(state, {ip :: inet:ip_address(),
		port :: inet:port_number(),
		identifier :: any(),
		limit :: pos_integer(),
		active_sockets=[] :: [{reference(), port()}]|[],
		idle_sockets=[] :: [{reference(), port()}]|[],
		tid :: ets:tid(),
		manager_timer :: reference()|undefined
	       }).

-define(IDLE_TIMEOUT, kolus_app:config(manager_idle_timeout)).
-define(SOCK_IDLE_TIMEOUT, kolus_app:config(socket_idle_timeout)).

% API
-export([start_link/3,
	 stop_sync/1,
	 get_socket/4,
	 return_socket/3,
	 return_unusable_socket/2]).

%% Callbacks
-export([init/1, handle_cast/2, handle_call/3,
	 handle_info/2, terminate/2, code_change/3]).

start_link(Identifier, Ip, Port) ->
    gen_server:start_link(?MODULE, [Identifier, Ip, Port], []).

% Adding this like this since I'll be removing the supervisor soon enough,
% don't see why you'd want to stop them (it's here for testing).
% @TODO: what to do with callers that might have connections checked out?
stop_sync(Pid) ->
    gen_server:call(Pid, stop).

get_socket(Pid, Identifier, Caller, Opts) ->
    Timeout = kolus_helper:get_key_or_default(timeout, Opts, 5000),
    gen_server:call(Pid, {get, Identifier, Caller}, Timeout).

return_socket(Pid, Ref, Socket) ->
    gen_server:cast(Pid, {return, Ref, Socket}).

return_unusable_socket(Pid, Ref) ->
    gen_server:cast(Pid, {return_unusable, Ref}).

init([Identifier, Ip, Port]) ->
    Tid = ets:new(kolus_managers, [set,protected]),
    true = gproc:reg(?LOOKUP_PID({Ip, Port}), Tid),
    Limit = kolus_app:config(endpoint_connection_limit),
    ets:insert(Tid, [{idle,0},{unused,Limit}]),
    {ok, #state{ip=Ip,port=Port,
		limit=Limit,identifier=Identifier,
		manager_timer=maybe_set_timer([], []),
		tid=Tid}}.

handle_cast({return, CallerMonitorRef, Socket}, #state{active_sockets=ActiveSockets,tid=Tid,
						       idle_sockets=IdleSockets,
						       manager_timer=MngrTimer}=State) ->
    FoundSocket = find_socket(CallerMonitorRef, ActiveSockets),
    {ActiveSockets0, IdleSockets0} = return_socket(Socket, FoundSocket, ActiveSockets, IdleSockets),
    increment_idle(Tid),
    cancel_timer(MngrTimer),
    {noreply, State#state{idle_sockets=IdleSockets0, active_sockets=ActiveSockets0,
			  manager_timer=undefined}};
handle_cast({return_unusable, CallerMonitorRef}, #state{active_sockets=ActiveSockets,
							idle_sockets=IdleSockets,
							manager_timer=Ref,
							tid=Tid}=State) ->
    FoundSocket = find_socket(CallerMonitorRef, ActiveSockets),
    ActiveSockets0 = remove_dead_socket(FoundSocket, ActiveSockets),
    increment_unused(Tid),
    cancel_timer(Ref),
    {noreply, State#state{active_sockets=ActiveSockets0,
			  manager_timer=maybe_set_timer(ActiveSockets0,IdleSockets)}};
    
handle_cast(_Msg, State) ->
    {noreply, State}.

% The caller has requested a socket but there are no idle, tell it to create one
% and return a monitor reference.
handle_call({get, Identifier, Caller}, _From, #state{idle_sockets=[],
						     ip=Ip,port=Port,tid=Tid,
						     active_sockets=Active,limit=Limit,
						     manager_timer=Ref,
						     identifier=Identifier}=State) when Limit > length(Active) ->
    CallerMonitorRef = erlang:monitor(process, Caller),
    decrement_unused(Tid),
    ActiveSockets0 = add_socket(CallerMonitorRef, undefined, Active),
    cancel_timer(Ref),
    {reply, {create, CallerMonitorRef, Ip, Port}, State#state{active_sockets=ActiveSockets0,
							      manager_timer=undefined}};

handle_call({get, Identifier, Caller}, _From,
            #state{idle_sockets = [{TimerRef, Socket} | Sockets],
                   active_sockets = Active, tid = Tid,
		   manager_timer = ManagerTimerRef,
                   identifier = Identifier} = State) ->
    cancel_timer(TimerRef),
    CallerMonitorRef = erlang:monitor(process, Caller),
    decrement_idle(Tid),
    ActiveSockets0 = add_socket(CallerMonitorRef,Socket, Active),
    ok = gen_tcp:controlling_process(Socket, Caller),
    cancel_timer(ManagerTimerRef),
    {reply, {socket, CallerMonitorRef, Socket}, State#state{active_sockets=ActiveSockets0,
							    idle_sockets=Sockets,
							    manager_timer=undefined}};
handle_call({get, Identifier, _}, _From, #state{identifier=Identifier0}=State) when
      Identifier /= Identifier0 ->
    % Not sure what to do here..
    {reply, rejected, State};
handle_call({get, _, _}, _From, State) ->
    {reply, rejected, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

% A socket has timed out, close it and remove
handle_info({timeout, TimerRef, close}, #state{idle_sockets=Idle,
					       active_sockets=Active,
					       manager_timer = Ref,
					       tid=Tid}=State) ->
    {_,Socket} = find_socket(TimerRef, Idle),
    ok = close_socket(Socket),
    decrement_idle(Tid),
    increment_unused(Tid),
    cancel_timer(Ref),
    Idle0 = remove_socket(TimerRef, Idle),
    Ref0 = maybe_set_timer(Idle0, Active),
    {noreply, State#state{idle_sockets = Idle0,
			  manager_timer = Ref0}};

handle_info({timeout, TimerRef, mngr_timeout}, #state{manager_timer = TimerRef,
						      idle_sockets=[],
						      active_sockets=[]}=State) ->
    {stop, normal, State};
% Should not happen, but lets stay safe
handle_info({timeout, TimerRef, mngr_timeout}, #state{manager_timer = TimerRef}=State) ->
    {noreply, State#state{manager_timer=undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    Reason.

code_change(_, State, _) ->
    {ok, State}.

% Internal
increment_idle(Tid) ->
    ets:update_counter(Tid, idle, 1).
decrement_idle(Tid) ->
    ets:update_counter(Tid, idle, -1).

increment_unused(Tid) ->
    ets:update_counter(Tid, unused, 1).
decrement_unused(Tid) ->
    ets:update_counter(Tid, unused, -1).

close_socket(Socket) ->
    gen_tcp:close(Socket).

find_socket(MonitorRef, List) ->
    lists:keyfind(MonitorRef, 1, List).

add_socket(MonitorRef, Socket, List) ->
    List ++ [{MonitorRef, Socket}].

remove_socket(MonitorRef, List) ->
    lists:keydelete(MonitorRef, 1, List).

remove_dead_socket({CallerMonitorRef,_}, Sockets) ->
    erlang:demonitor(CallerMonitorRef, [flush]),
    remove_socket(CallerMonitorRef, Sockets).
    
return_socket(_, false, ActiveSockets, IdleSockets) ->
    {ActiveSockets, IdleSockets};
return_socket(Socket, {CallerMonitorRef, _}, ActiveSockets, IdleSockets) ->
    erlang:demonitor(CallerMonitorRef, [flush]),
    TimerRef = erlang:start_timer(?SOCK_IDLE_TIMEOUT, self(), close),
    {remove_socket(CallerMonitorRef, ActiveSockets),
     IdleSockets++[{TimerRef, Socket}]}.

maybe_set_timer([], []) ->
    erlang:start_timer(?IDLE_TIMEOUT, self(), mngr_timeout);
maybe_set_timer(_, _) ->
    undefined.

cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            %% Flush expired timer message
            receive
                {timeout, Ref, _} -> ok
            after 0 -> ok
            end;
        _Time ->
            %% Timer didn't fire, so no message to worry about
            ok
    end.
