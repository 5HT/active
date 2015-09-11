-module(active_events).
-include("active.hrl").
-behaviour(gen_event).
-export([start_link/0, subscribe_onload/1, subscribe_onnew/1, notify_reload/1, subscribe/2]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

start_link() -> gen_event:start_link({local, ?MODULE}).

subscribe_onload(Function) -> subscribe(reloaded, Function).
subscribe_onnew(Function) -> subscribe(loaded_new, Function).
subscribe(Event, Function) -> ok = gen_event:add_sup_handler(?MODULE, {?MODULE, Event}, [Event, Function]).
notify_reload(Event) -> gen_event:notify(?MODULE, Event).

init([Event, Function]) -> {ok, #event_state{event = Event, function = Function}}.
handle_event({Event, Module}, State = #event_state{event = Event, function = {Mod, Fun}}) -> erlang:apply(Mod, Fun, [[Module]]), {ok, State};
handle_event(_, State) -> {ok, State}.
handle_call(_Request, _State) -> erlang:error(not_implemented).
handle_info(_Info, _State) -> erlang:error(not_implemented).
terminate(_Args, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
