-module(kolus_managers_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

start_link(Tid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Tid]).

init([Tid]) ->
    Manager = ?CHILD(kolus_manager, worker, [Tid]),
    {ok, { {simple_one_for_one, 0, 1}, [Manager]} }.
