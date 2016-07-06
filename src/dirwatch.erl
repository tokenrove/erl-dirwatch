-module(dirwatch).

-export([start/2, start/3, stop/1]).

-type handle() :: pid().

-record(state,
        {pid :: pid(),
         port :: port(),
         cooldown_ms :: timer:time(),
         timer :: timer:tref()}).


priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            filename:join(filename:dirname(filename:dirname(code:which(?MODULE))),
                          "priv");
        Dir -> Dir
    end.


%% If you put this in on_load, it will immediately get unloaded when
%% that child dies.
load() ->
    case erl_ddll:load_driver(priv_dir(), "dirwatch") of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Message} ->
            error_logger:error_msg("dirwatch: error loading driver: ~p",
                                   [erl_ddll:format_error(Message)])
    end.


-spec start(pid(), file:name_all()) -> {ok,handle()} | {error,_}.

%% Default cooldown of 5 seconds
start(Self, Path) -> start(Self, Path, 5000).


-spec start(pid(), file:name_all(), timer:time()) -> {ok,handle()} | {error,_}.

start(Self, Path, CooldownMs) ->
    Pid = spawn_link(fun () -> new_watcher(Self, Path, CooldownMs) end),
    {ok, Pid}.


-spec stop(handle()) -> ok | {error,_}.

stop(Handle) ->
    Handle ! terminate,
    ok.


new_watcher(Pid, Path, CooldownMs) ->
    ok = load(),
    Port = open_port({spawn_driver, "dirwatch "++Path}, [in]),
    watch(#state{pid=Pid, port=Port, cooldown_ms=CooldownMs}).


bump_timer(undefined, Ms) ->
    {ok, U} = timer:send_after(Ms, cool),
    U;
bump_timer(T, Ms) ->
    timer:cancel(T),
    bump_timer(undefined, Ms).


-spec watch(#state{}) -> ok.

watch(S=#state{
           pid = Pid,
           port = Port,
           cooldown_ms = CooldownMs,
           timer = T
      }) ->
    receive
        {Port, ok} ->
            watch(S#state{timer=bump_timer(T, CooldownMs)});
        cool ->
            Pid ! {dirwatch, self(), changed},
            watch(S#state{timer=undefined});
        terminate ->
            ok;
        _ ->
            exit(unexpected_message)
    end.
