-module(active_app).
-behaviour(application).
-behaviour(supervisor).
-export([start/2, stop/1, init/1]).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

stop(_State) -> ok.
start(_StartType, _StartArgs) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([]) -> {ok, { {one_for_one, 5, 10}, [ ?CHILD(active, worker, []),
                                           ?CHILD(active_events, worker, []) ]}}.
