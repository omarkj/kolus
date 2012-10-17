-module(kolus_director).
-behaviour(gen_server).

% API
-export([start_link/0,
	 create_manager/3]).

%% Callbacks
-export([init/1, handle_cast/2, handle_call/3,
	 handle_info/2, terminate/2, code_change/3]).

-include("kolus_internal.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_manager(Identifier, Ip, Port) ->
    gen_server:call(?MODULE, {create, Identifier, Ip, Port}).

init([]) ->
    {ok, undefined}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({create, Identifier, Ip, Port}, _From, State) ->
    Pid = case gproc:lookup_pids(?LOOKUP_PID({Ip,Port})) of
	      [Pid0] ->
		  Pid0;
	      [] ->
		  {ok, Pid0} = kolus_manager:start_link(Identifier, Ip, Port),
		  Pid0
	  end,
    {reply, Pid, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    Reason.

code_change(_, State, _) ->
    {ok, State}.




