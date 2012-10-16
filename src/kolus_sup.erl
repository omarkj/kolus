-module(kolus_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Tid = ets:new(kolus_managers, [set,{read_concurrency,true}]),
    ManagersSup = ?CHILD(kolus_managers_sup, supervisor, [Tid]),
    {ok, { {one_for_one, 5, 10}, [ManagersSup]} }.
