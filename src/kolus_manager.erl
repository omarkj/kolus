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
		closed=false::boolean()
	       }).

-define(IDLE_TIMEOUT, kolus_app:config(manager_idle_timeout)).

% API
-export([start_link/3,
	 get_socket/4,
	 return_socket/3]).

%% Callbacks
-export([init/1, handle_cast/2, handle_call/3,
	 handle_info/2, terminate/2, code_change/3]).

start_link(Identifier, Ip, Port) ->
    gen_server:start_link(?MODULE, [Identifier, Ip, Port], []).

get_socket(Pid, Identifier, Caller, Opts) ->
    Timeout = kolus_helper:get_key_or_default(timeout, Opts, 5000),
    gen_server:call(Pid, {get, Identifier, Caller}, Timeout).

return_socket(Pid, Ref, Socket) ->
    ok = gen_tcp:controlling_process(Socket, Pid),
    gen_server:cast(Pid, {return, Ref, Socket}).

init([Identifier, Ip, Port]) ->
    Tid = ets:new(kolus_managers, [set,protected]),
    true = gproc:reg(?LOOKUP_PID({Ip, Port}), Tid),
    Limit = kolus_app:config(endpoint_connection_limit),
    ets:insert(Tid, [{idle,0},{unused,Limit}]),
    {ok, #state{ip=Ip,port=Port,
		limit=Limit,identifier=Identifier,
		tid=Tid}, ?IDLE_TIMEOUT}.

handle_cast({return, CallerMonitorRef, Socket}, #state{active_sockets=ActiveSockets,tid=Tid,
						       idle_sockets=IdleSockets}=State) ->
    FoundSocket = find_socket(CallerMonitorRef, ActiveSockets),
    {ActiveSockets0, IdleSockets0} = return_socket(Socket, FoundSocket, ActiveSockets, IdleSockets),
    increment_idle(Tid),
    {noreply, State#state{idle_sockets=IdleSockets0, active_sockets=ActiveSockets0}};
handle_cast(_Msg, State) ->
    {noreply, State}.

% The caller has requested a socket but there are no idle, tell it to create one
% and return a monitor reference.
handle_call({get, Identifier, Caller}, _From, #state{idle_sockets=[],
						     ip=Ip,port=Port,tid=Tid,
						     active_sockets=Active,limit=Limit,
						     identifier=Identifier}=State) when Limit > length(Active) ->
    CallerMonitorRef = erlang:monitor(process, Caller),
    decrement_unused(Tid),
    {reply, {create, CallerMonitorRef, Ip, Port}, State#state{active_sockets=add_socket(CallerMonitorRef, undefined, Active)}};

handle_call({get, Identifier, Caller}, _From,
            #state{idle_sockets = [{TimerRef, Socket} | Sockets],
                   active_sockets = Active, tid = Tid,
                   identifier = Identifier} = State) ->
    cancel_timer(TimerRef),
    CallerMonitorRef = erlang:monitor(process, Caller),
    decrement_idle(Tid),
    ActiveSockets0 = add_socket(CallerMonitorRef,Socket, Active),
    ok = gen_tcp:controlling_process(Socket, Caller),
    {reply, {socket, CallerMonitorRef, Socket}, State#state{active_sockets=ActiveSockets0,
							    idle_sockets=Sockets}};
handle_call({get, _, _}, _From, State) ->
    {reply, rejected, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

% A socket has timed out, close it and remove
handle_info({timeout, TimerRef, close}, #state{idle_sockets=Idle,tid=Tid}=State) ->
    Socket = find_socket(TimerRef, Idle),
    ok = close_socket(Socket),
    decrement_idle(Tid),
    increment_unused(Tid),
    {noreply, State#state{idle_sockets=remove_socket(TimerRef, Idle)}};

handle_info(timeout, State) ->
    {stop, normal, State};

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

return_socket(_, false, ActiveSockets, IdleSockets) ->
    {ActiveSockets, IdleSockets};
return_socket(Socket, {CallerMonitorRef, _}, ActiveSockets, IdleSockets) ->
    erlang:demonitor(CallerMonitorRef),
    TimerRef = erlang:start_timer(kolus_helper:get_env(socket_timeout), self(), close),
    {remove_socket(CallerMonitorRef, ActiveSockets),
     IdleSockets++[{TimerRef, Socket}]}.

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
    end
