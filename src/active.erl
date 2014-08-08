-module(active).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {last, root}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> fs:subscribe(), erlang:process_flag(priority, low), {ok, #state{last=fresh, root=fs:path()}}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info({_Pid, {fs,file_event}, {Path, Flags}}, #state{root=Root} = State) ->
    Cur = path_shorten(filename:split(Root)),
    P = filename:split(Path),

    Result = case lists:prefix(Cur, P) of
        true ->
            Components = P -- Cur,
            %error_logger:info_msg("event: ~p ~p", [Components, Flags]),
            path_event(Components, Flags, State);
        false ->
            ok
    end,

    {noreply, State#state{last={event, Path, Flags, Result}}};
handle_info({load_ebin, Atom}, State) -> do_load_ebin(Atom), {noreply, State#state{last={do_load_ebin, Atom}}};
handle_info(Info, State) -> {noreply, State#state{last={unk, Info}}}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

path_event(C, [E|_Events], _State) when E =:= created; E =:= modified; E =:= renamed ->
    case path_filter(C) of true -> otp(C); false -> ignore end;
path_event(C, [_E|Events], State) -> path_event(C, Events, State);
path_event(_, [], _State) -> done.

otp(["deps",App|Rest]) -> app(App,Rest);
otp(["apps",App|Rest]) -> app(App,Rest);
otp([Some|Path]) -> app(top(),[Some|Path]);
otp(_) -> ok.

app(App,["ebin",Module|_]) -> load_ebin(App,Module);
app(App,["priv","fdlink"++_]) -> skip;
app(App,["priv","mac"++_]) -> skip;
app(App,["priv","windows"++_]) -> skip;
app(App,["priv","linux"++_]) -> skip;
app(App,["priv"|_]) -> compile(App);
app(App,["include"|_]) -> compile(App);
app(App,["src"|_]) -> compile(App);
app(_,_)-> ok.

top() -> lists:last(filename:split(filename:absname(""))).

compile(_State) ->
    put(mode,active),
    try mad:main(["compile"]) catch E:R -> io:format("Catch: ~p:~p~n",[E,R]) end.

load_ebin(App,EName) ->
    Tokens = string:tokens(EName, "."),
    case Tokens of
        [Name, "beam"] -> do_load_ebin(list_to_atom(Name));
        [Name, "bea#"] ->
            case monitor_handles_renames() of
                false ->
                    erlang:send_after(500, ?SERVER, {load_ebin, list_to_atom(Name)}),
                    delayed;
                true ->
                    ignored
            end;
        %[Name, Smth] -> ok;
        _ ->
            %error_logger:warning_msg("Active: unknown BEAM file: ~p", [EName]),
            ok
    end.

do_load_ebin(Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    code:load_binary(Module, Filename, Binary),
    error_logger:info_msg("Active: module loaded: ~p~n", [Module]),
    reloaded.

monitor_handles_renames([renamed|_]) -> true;
monitor_handles_renames([_|Events]) -> monitor_handles_renames(Events);
monitor_handles_renames([]) -> false.

monitor_handles_renames() ->
    case get(monitor_handles_renames) of
        undefined ->
            R = monitor_handles_renames(fs:known_events()),
            put(monitor_handles_renames, R),
            R;
        V -> V
    end.

% ["a", "b", ".."] -> ["a"]
path_shorten(Coms) -> path_shorten_r(lists:reverse(Coms), [], 0).

path_shorten_r([".."|Rest], Acc, Count) -> path_shorten_r(Rest, Acc, Count + 1);
path_shorten_r(["."|Rest], Acc, Count) -> path_shorten_r(Rest, Acc, Count);
path_shorten_r([_C|Rest], Acc, Count) when Count > 0 -> path_shorten_r(Rest, Acc, Count - 1);
path_shorten_r([C|Rest], Acc, 0) -> path_shorten_r(Rest, [C|Acc], 0);
path_shorten_r([], Acc, _) -> Acc.

%
% Filters
%

path_filter(L) -> not lists:any(fun(E) -> not path_filter_dir(E) end, L) andalso path_filter_last(lists:last(L)).

path_filter_dir(".git") -> false;
path_filter_dir(".hg")  -> false;
path_filter_dir(".svn") -> false;
path_filter_dir("CVS")  -> false;
path_filter_dir("log")  -> false;
path_filter_dir(_)      -> true.

path_filter_last(".rebarinfo")     -> false;   % new rebars
path_filter_last("LICENSE")        -> false;
path_filter_last("4913 (deleted)") -> false;   % vim magical file
path_filter_last("4913")           -> false;
path_filter_last(_)                -> true.
