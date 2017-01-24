-module(dirwatch).

-export([start/2, start/3, stop/1]).

-type handle() :: pid().

-record(state,
        {pid :: pid(),
         port :: port()}).


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
        ok -> driver;
        {error, already_loaded} -> driver;
        {error, Message} ->
            error_logger:error_msg("dirwatch: falling back to polling;"
                                   ++" failed to load driver: ~p",
                                   [erl_ddll:format_error(Message)]),
            poll
    end.

open(Path, CooldownMs) ->
    case load() of
        driver ->
            open_port({spawn_driver,
                       ["dirwatch ", integer_to_list(CooldownMs, 10), $\s, Path]},
                      [in]);
        poll ->
            spawn_link(fun () -> polling_loop(self(), CooldownMs) end)
    end.

polling_loop(Pid, CooldownMs) ->
    erlang:send_after(CooldownMs, Pid, {self(), ok}),
    polling_loop(Pid, CooldownMs).

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
    Port = open(Path, CooldownMs),
    watch(#state{pid=Pid, port=Port}).


-spec watch(#state{}) -> ok.

watch(S=#state{
           pid = Pid,
           port = Port
      }) ->
    receive
        {Port, ok} ->
            Pid ! {dirwatch, self(), changed},
            watch(S);
        terminate ->
            ok;
        _ ->
            exit(unexpected_message)
    end.
