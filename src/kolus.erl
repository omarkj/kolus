-module(kolus).

-export([status/1,
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
-type backend_info() :: {limit, pos_integer()}|{idle, pos_integer()}.
-type backend_status() :: {backend(), pid(), [backend_info()]}.
-type connect_opts() :: {timeout, pos_integer()}.

-spec get_socket(kolus_socket()) -> port().
get_socket(#kolus_socket{socket=Socket}) ->
    Socket.

-spec get_manager(kolus_socket()) -> pid().
get_manager(#kolus_socket{manager=Manager}) ->
    Manager.

-spec status([backend()]) -> [backend_status()]|[].
status(Backends) ->
    check_backends(Backends).

-spec connect(any(), pid()|backend()) ->
		     {ok, kolus_socket()}.
connect(Opaque, Backend) ->
    connect(Opaque, Backend, []).

-spec connect(any(), pid()|backend(), [connect_opts()]) ->
		     {ok, kolus_socket()}.
connect(Identifier, Pid, Opts) when is_pid(Pid) ->
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
    end;
connect(Identifier, {Ip, Port}, Opts) ->
    case gproc:lookup_pids(?LOOKUP_PID({Ip,Port})) of
	[Pid] ->
	    % Someone beat us to creating a manager
	    connect(Identifier, Pid, Opts);
	[] ->
	    % Lets create a manager
	    Pid = kolus_director:create_manager(Identifier, Ip, Port),
	    connect(Identifier, Pid, Opts)
    end.

-spec return(#kolus_socket{}) -> ok.
return(#kolus_socket{socket=Socket,manager=Manager,ref=Ref}=KSocket) ->
    case gen_tcp:controlling_process(Socket, Manager) of
	{error, closed} ->
	    finished(KSocket);
	ok ->
	    kolus_manager:return_socket(Manager, Ref, Socket)
    end.

-spec finished(#kolus_socket{}) -> ok.
finished(#kolus_socket{manager=Manager,ref=Ref}) ->
    kolus_manager:return_unusable_socket(Manager, Ref).

% Internal
check_backends(Backends) ->
    check_backends(Backends, []).

check_backends([], Res) ->
    Res;
check_backends([Backend|Backends], Res) ->
    Status = get_status(Backend, gproc:lookup_values(?LOOKUP_PID(Backend))),
    check_backends(Backends, Res ++ Status).

get_status(_,[]) ->
    [];
get_status(Backend,[{Pid, Tid}]) ->
    [{Backend, Pid,ets:tab2list(Tid)}].

create_connection(Ip, Port) ->
    case gen_tcp:connect(Ip, Port, [{active, false}]) of
	{ok, Socket} ->
	    Socket;
	{error, Error} ->
	    {error, Error}
    end.
