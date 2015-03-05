%%%-------------------------------------------------------------------
%%% @author lol4t0
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Mar 2015 15:22
%%%-------------------------------------------------------------------
-module(active_events).
-author("lol4t0").
-behaviour(gen_event).

%% API
-export([start_link/0, subscribe_onload/1, subscribe_onnew/1, notify_reload/1, subscribe/2]).

%% Callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% API impl
start_link() ->
    gen_event:start_link({local, ?MODULE}).

-type mf() :: {M :: module(), F :: atom()}.
-spec subscribe_onload(Function :: mf()) -> ok.
subscribe_onload(Function) ->
    subscribe(reloaded, Function).

-spec subscribe_onnew(Function :: mf()) -> ok.
subscribe_onnew(Function) ->
    subscribe(loaded_new, Function).

-spec subscribe(Event :: reloaded | loaded_new, Function :: mf()) -> ok.
subscribe(Event, Function) ->
    ok = gen_event:add_sup_handler(?MODULE, {?MODULE, Event}, [Event, Function]).

notify_reload(Event) ->
    gen_event:notify(?MODULE, Event).


%% Callbacks impl
-record(state, {
    event :: reloaded | loaded_new,
    function :: mf()
}).

init([Event, Function]) ->
    {ok, #state{event = Event, function = Function}}.

handle_event({Event, Module}, State = #state{event = Event, function = {Mod, Fun}}) ->
    erlang:apply(Mod, Fun, [[Module]]),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.


handle_call(_Request, _State) ->
    erlang:error(not_implemented).

handle_info(_Info, _State) ->
    erlang:error(not_implemented).

terminate(_Args, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private dunctions

