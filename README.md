
**You probably don't want to use this.**


Minimal directory watching for Erlang; just enough to support
`bertconf`.

That is, for each directory specified, we watch only this directory
for move, create, and delete events, and do not setup recursive
watches.

Uses `inotify` on Linux.

When ported to BSDs, it will use `kqueue` on BSDs.  Does _not_ use
`fsevents` on OS X, and probably never will: the only exposed API
requires you to use a `CFRunLoop`, even though I'm sure there's a nice
UNIX-y `fd` in there somewhere.

It would have been nice to do this as a NIF that returns an fd from
inotify and used `open_port({fd, ...` and Erlang's binary matching to
parse inotify messages, but this wouldn't permit us to handle other
OSes where less-UNIX-y approaches are used.  Instead we use a
linked-in driver which allows us to do the right thing for inotify,
and still offers some portability options.

== Building

```
rebar3 compile
```

== Usage

=== `dirwatch:start(Self, Path, CooldownMs = 5000) -> {ok,Handle} | {error,_}`

Start watching the files in `Path`.  After events have occurred, will
wait `CooldownMs` before sending a message of the form
`{dirwatch,Handle,changed}` to the `Self` process.

The dirwatch process referred to by `Handle` monitors `Self` and
terminates if it terminates.

=== `dirwatch:stop(Handle) -> ok | {error,_}`

Explicitly stops a dirwatch process.
