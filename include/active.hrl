
-type mf() :: {M :: module(), F :: atom()}.
-spec subscribe_onload(Function :: mf()) -> ok.
-spec subscribe_onnew(Function :: mf()) -> ok.
-spec subscribe(Event :: reloaded | loaded_new, Function :: mf()) -> ok.
-record(event_state, {
    event :: reloaded | loaded_new,
    function :: mf()
}).

