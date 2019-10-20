-module(active).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {last, root}).
-export([ver/0, start_link/0, init/1, handle_call/3,
         handle_cast/2, handle_info/2, terminate/2, code_change/3]).

ver() -> 3.

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    fs:subscribe(),
    erlang:process_flag(priority, low),
    gen_server:cast(self(), recompile_all),
    {ok, #state{last=fresh, root=fs:path()}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(recompile_all, State) ->
   compile(top(), ["all"], []), {noreply, State}.
handle_info({_Pid, {fs,file_event}, {Path, Flags}}, #state{root=Root} = State) ->

    Cur = path_shorten(filename:split(Root)),
    P = filename:split(Path),
    Result = case lists:prefix(Cur, P) of
        true -> path_event(P -- Cur, Flags, State, Path);
        false -> ok
    end,

    {noreply, State#state{last={event, Path, Flags, Result}}};

handle_info({load_ebin, Atom}, State) ->
    do_load_ebin(Atom),
    {noreply, State#state{last={do_load_ebin, Atom}}};

handle_info(Info, State) -> {noreply, State#state{last={unk, Info}}}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

path_event(C, [E|_], _, Path)
    when E =:= created;
         E =:= modified;
         E =:= renamed ->
    case path_filter(C) of
         true  -> maybe_otp(C, Path);
         false -> ignore end;
path_event(C, [_E|Events], State, Path) ->
    path_event(C, Events,  State, Path);
path_event(_, [], _, _) ->
    done.

maybe_otp(C, Path) ->
    case application:get_env(active, handler) of
         {ok,{M,F}} -> M:F(C);
                  _ -> otp(C,Path) end.

otp(["deps",App|Rest],Path) -> maybe_app(App,Rest,Path);
otp(["apps",App|Rest],Path) -> maybe_app(App,Rest,Path);
otp([Some|Rest],Path) -> maybe_app(top(),[Some|Rest],Path);
otp(_,_) -> ok.

maybe_app(App, SplitPath, Path) ->
    EnabledApps = application:get_env(active, apps, undefined),
    case EnabledApps of
        undefined -> app(App, SplitPath, Path);
        {ok,L} when is_list(L) ->
            AppAtom = list_to_atom(App),
            case lists:member(AppAtom, L) of
                true -> app(App, SplitPath, Path);
                false -> skip
            end
    end.

app( App,["ebin",Module|_],_)       -> load_ebin(App,Module);
app(_App,["priv","fdlink"++_],_)    -> skip;
app(_App,["priv","mac"++_],_)       -> skip;
app(_App,["priv","windows"++_],_)   -> skip;
app(_App,["priv","linux"++_],_)     -> skip;
app(_App,["priv","static"|_Rest],Path) -> compile_skip(compile_on_static,_App,_Rest,Path);
app( App,["priv"|Rest],Path)           -> compile_skip(compile_on_priv,App,Rest,Path);
app( App,["include"|Rest],Path)        -> compile(App,Rest,Path);
app( App,["src"|Rest],Path)            -> compile(App,Rest,Path);
app( App,["lib"|Rest],Path)            -> compile(App,Rest,Path);
app(_,_,Path)-> ok.

compile_skip(Key,App,Rest,Path) ->
  case application:get_env(active,Key,false) of
       false -> skip;
           _ -> compile(App,Rest,Path) end.

top() -> lists:last(filename:split(fs:path())).

compile(_App, _, []) -> ok;
compile(_App, [],Path) -> ok;
compile(App,Rest,Path) ->
    case lists:last(Rest) of
         ".#" ++ _ -> skip;
             _ -> try put(App,updated),
                      {M,F} = application:get_env(active,compile,{mad,compile}),
                      M:F(App,Path)
                catch E:R ->
                      io:format("~p", [erlang:get_stacktrace()]),
                      io:format("Catch: ~p:~p",[E,R]) end end.

load_ebin(_App, EName) ->
    case lists:reverse(EName) of
        "maeb." ++ Tail -> Name = lists:reverse(Tail),
            LoadRes = do_load_ebin(list_to_atom(lists:flatten(Name))),
            io:format("Active: module loaded: ~p~n\n\r", [LoadRes]),
            active_events:notify_reload(LoadRes);
        "#aeb." ++ _ -> ok;
        _ -> io:format("Active: unknown BEAM file: ~p", [EName]), ok
    end.

do_load_ebin(Module) ->
    IsLoaded = case code:is_loaded(Module) of
                   {file, _} ->
                       true;
                   false ->
                       false
               end,
    {Module, Binary, Filename} = code:get_object_code(Module),
    case code:load_binary(Module, Filename, Binary) of
        {module, Module} when IsLoaded->
            {reloaded, Module};
        {module, Module} when not IsLoaded ->
            {loaded_new, Module};
        {error, Reason} ->
            {load_error, Module, Reason}
    end.

% ["a", "b", ".."] -> ["a"]
path_shorten(Coms) -> path_shorten_r(lists:reverse(Coms), [], 0).

path_shorten_r([".."|Rest], Acc, Count) -> path_shorten_r(Rest, Acc, Count + 1);
path_shorten_r(["."|Rest], Acc, Count) -> path_shorten_r(Rest, Acc, Count);
path_shorten_r([_C|Rest], Acc, Count) when Count > 0 -> path_shorten_r(Rest, Acc, Count - 1);
path_shorten_r([C|Rest], Acc, 0) -> path_shorten_r(Rest, [C|Acc], 0);
path_shorten_r([], Acc, _) -> Acc.

path_filter(L) ->
    not lists:any(fun(E) -> not path_filter_dir(E) end, L)
        andalso path_filter_file(lists:last(L))
        andalso path_filter_ext(ext(L)).

ext(L) -> filename:extension(lists:last(L)).

path_filter_dir(".git")            -> false;
path_filter_dir(".hg")             -> false;
path_filter_dir(".svn")            -> false;
path_filter_dir("CVS")             -> false;
path_filter_dir("ebin")            -> false;
path_filter_dir("build")           -> false;
path_filter_dir("_build")          -> false;
path_filter_dir("log")             -> false;
path_filter_dir("node_modules")    -> false;
path_filter_dir(_)                 -> true.

path_filter_file(".rebarinfo")     -> false;
path_filter_file("LICENSE")        -> false;
path_filter_file("4913 (deleted)") -> false;
path_filter_file("4913")           -> false;
path_filter_file(_)                -> true.

path_filter_ext(".app")            -> false;
path_filter_ext(".jpg")            -> false;
path_filter_ext(".png")            -> false;
path_filter_ext(".gif")            -> false;
path_filter_ext(_)                 -> true.
