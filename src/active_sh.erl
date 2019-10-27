-module(active_sh).
-compile(export_all).

reduce() -> fun({_, Chunk}, Acc) -> [Chunk|Acc] end.

run(Args) ->
    erlang:open_port({spawn_executable, os:find_executable("sh")},
        [stream, in, out, eof, use_stdio, stderr_to_stdout, binary, exit_status,
            {args, ["-c",Args]}, {cd, element(2,file:get_cwd())}, {env, []}]).

sh(Port) -> sh(Port, reduce(), []).
sh(Port, Fun, Acc) ->
    receive
        {Port, {exit_status, Status}} -> {done, Status, iolist_to_binary(lists:reverse(Acc))};
        {Port, {data, {eol, Line}}}   -> sh(Port, Fun, Fun({eol,   Line}, Acc));
        {Port, {data, {noeol, Line}}} -> sh(Port, Fun, Fun({noeol, Line}, Acc));
        {Port, {data, Data}}          -> sh(Port, Fun, Fun({data,Data}, Acc))
    end.

exe(X) -> sh(run(X)).

