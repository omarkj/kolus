-module(kolus_managers_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
	 create_manager/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create_manager(Identifier, Ip, Port) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [Identifier, Ip, Port]),
    Pid.

init([]) ->
    Manager = ?CHILD(kolus_manager, worker),
    {ok, { {simple_one_for_one, 0, 1}, [Manager]} }.
