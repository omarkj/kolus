-module(kolus).

-export([status/1,
	 select/3,
	 connect/2,
	 return/1,
	 finished/1,
	 get_socket/1,
	 get_manager/1]).

-include("kolus_internal.hrl").
-include("kolus.hrl").

-record(kolus_socket, {ref :: reference(),
		       socket :: port(),
		       manager :: pid()}).

-opaque kolus_socket() :: #kolus_socket{}.
-type backend() :: {inet:ip_address(), inet:port_number()}.
-type kolus_backend() :: #kolus_backend{}.
-type kolus_connect_opts() :: {caller, pid()}.

-export_type([backend/0,
	      kolus_backend/0]).

-spec get_socket(kolus_socket()) -> port().
get_socket(#kolus_socket{socket=Socket}) ->
    Socket.

-spec get_manager(kolus_socket()) -> pid().
get_manager(#kolus_socket{manager=Manager}) ->
    Manager.

-spec status([backend()|kolus_backend()]|[]) ->
		    [kolus_backend()]|[].
status(Backends) ->
    check_backends(Backends).

-spec select(any(), [kolus_backend()], function()) -> {socket, kolus_socket()}|
						      {error, rejected}.
select(Opaque, Backends, SelectFun) ->
    connect(Opaque, SelectFun(check_backends(Backends))).

-spec connect(any(), backend()|kolus_backend()) ->
		     {ok, kolus_socket()}.
connect(Opaque, Backend) ->
    connect(Opaque, Backend, []).

-spec connect(any(), backend()|kolus_backend(), [kolus_connect_opts()]|[]) ->
		     {ok, kolus_socket()}|{error, rejected}.
connect(Identifier, {_, _}=IpPort, Opts) ->
    connect_(Identifier, IpPort, Opts);
connect(Identifier, #kolus_backend{manager=undefined,
				   ip=Ip,
				   port=Port}, Opts) ->
    connect_(Identifier, {Ip, Port}, Opts);
connect(Identifier, #kolus_backend{manager=Pid}, Opts) when is_pid(Pid) ->
    connect_(Identifier, Pid, Opts).

-spec return(#kolus_socket{}) -> ok.
return(#kolus_socket{socket=Socket,manager=Manager,ref=Ref}=KSocket) ->
    case erlang:is_process_alive(Manager) of
	true ->
	    try gen_tcp:controlling_process(Socket, Manager) of
		{error, closed} ->
		    finished(KSocket);
		ok ->
		    kolus_manager:return_socket(Manager, Ref, Socket)
	    catch
		error:function_clause ->
		    % An error came up returning the socket - the most likely
		    % reason is that the remote of the socket is refusing connections
		    % but the socket is in active false. Mark it as dead.
		    finished(KSocket)
	    end;
	_ ->
	    gen_tcp:close(Socket),
	    ok
    end.

-spec finished(#kolus_socket{}) -> ok.
finished(#kolus_socket{manager=Manager,ref=Ref}) ->
    kolus_manager:return_unusable_socket(Manager, Ref).

% Internal
connect_(Identifier, {Ip, Port}, Opts) ->
    case kolus_manager:start_link(Identifier, Ip, Port) of
	{ok, Pid} ->
	    connect_(Identifier, Pid, Opts);
	{error, {already_started, Pid}} ->
	    connect_(Identifier, Pid, Opts)
    end;
connect_(Identifier, Pid, Opts) ->
    Caller = kolus_helper:get_key_or_default(caller, Opts, self()),
    case kolus_manager:get_socket(Pid, Identifier, Caller, Opts) of
	{create, Ref, Ip, Port} ->
	    % No idle socket, create a new one
	    Socket = create_connection(Ip, Port),
	    {socket, #kolus_socket{ref=Ref,
				   manager=Pid,
				   socket=Socket}};
	{socket, Ref, Socket} ->
	    {socket, #kolus_socket{ref=Ref,
				   manager=Pid,
				   socket=Socket}};
	rejected ->
	    % Got rejected
	    {error, rejected}
    end.

check_backends(Backends) ->
    check_backends(Backends, []).

check_backends([], Res) ->
    Res;
check_backends([Backend|Backends], Res) ->
    check_backends(Backends, Res ++ [check_backend(Backend)]).

check_backend(#kolus_backend{manager=undefined,
			       ip=Ip,port=Port}) ->
    case gproc:lookup_values(?LOOKUP_PID({Ip, Port})) of
	[] ->
	    #kolus_backend{ip=Ip,port=Port};
	[{Pid,Tid}] ->
	    check_backend(Ip, Port, Pid, Tid)
    end;
check_backend(#kolus_backend{manager=Pid,manager_status=Tid,
			       ip=Ip,port=Port}) ->
    case erlang:is_process_alive(Pid) of
	true ->
	    check_backend(Ip, Port, Pid, Tid);
	false ->
	    #kolus_backend{ip=Ip,port=Port}
    end;
check_backend({Ip,Port}=Backend) ->
    case gproc:lookup_values(?LOOKUP_PID(Backend)) of
	[] ->
	    #kolus_backend{ip=Ip,
			   port=Port};
	[{Pid, Tid}] ->
	    [{idle,Idle}, {unused,Unused}] = ets:tab2list(Tid),
	    #kolus_backend{ip=Ip,
			   port=Port,
			   idle=Idle,
			   unused=Unused,
			   manager=Pid,
			   manager_status=Tid}
    end.

check_backend(Ip, Port, Pid, Tid) ->
    [{idle,Idle}, {unused,Unused}] = ets:tab2list(Tid),
    #kolus_backend{ip=Ip,
		   port=Port,
		   manager=Pid,
		   idle=Idle,
		   unused=Unused}.
    
create_connection(Ip, Port) ->
    case gen_tcp:connect(Ip, Port, [{active, false}]) of
	{ok, Socket} ->
	    Socket;
	{error, Error} ->
	    {error, Error}
    end.
