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

path_event(C, [E|_Events], State) when E =:= created; E =:= modified; E =:= renamed ->
    case path_filter(C) of
        true -> path_modified_event(C, State);
        false -> ignore end;
path_event(C, [_E|Events], State) -> path_event(C, Events, State);
path_event(_, [], _State) -> done.

path_modified_event([P, Name|Px] = _Path, State) when P =:= "apps"; P =:= "deps" -> app_modified_event(Name, Px, State);
path_modified_event([D|Px] = _Path, State) -> app_modified_event(toplevel_app(), [D|Px], State);
path_modified_event(_, _State) -> dont_care.

app_modified_event(_App, ["ebin", EName|_] = _Path, _State) -> load_ebin(EName);
app_modified_event(_App, ["src", _EName|_] = _Path, State) -> compile(State);
app_modified_event(_App, ["priv", _EName|_] = _Path, State) -> compile(State);
app_modified_event(_App, _P, _State) -> dont_care.

toplevel_app() -> lists:last(filename:split(filename:absname(""))).

compile(#state{root=_Root} = _State) ->
    mad:main(["compile"]).

load_ebin(EName) ->
    Tokens = string:tokens(EName, "."),
    case Tokens of
        [Name, "beam"] ->
            do_load_ebin(list_to_atom(Name));
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
            error_logger:warning_msg("Active: unknown BEAM file: ~p", [EName]),
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
