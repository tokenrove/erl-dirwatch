/* An Erlang driver for just enough kqueue
 *
 * Part of erl-dirwatch
 */

#include <dirent.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

#include "ei.h"
#include "erl_driver.h"

#include "common.h"
#include "macrology.h"


struct instance {
    ErlDrvPort port;
    int kq, fd;
    bool needs_rescan_p;
    size_t max_fds_seen;
    unsigned n_cooldowns;
    unsigned long cooldown;
};

/* Arbitrary; number of times we can reset the cooldown counter
   without allowing it to timeout. */
enum { MAX_COOLDOWNS = 12 };


static const int event_open_flags =
#ifdef O_EVTONLY
    O_EVTONLY
#else
    O_RDONLY
#endif
    ;



static bool is_watched(struct instance *me, int fd)
{
    struct kevent in, out;
    struct timespec ts = {0};
    EV_SET(&in, fd, EVFILT_VNODE, EV_RECEIPT, 0, 0, 0);
    int rv = kevent(me->kq, &in, 1, &out, 1, &ts);
    if (out.ident != (uintptr_t)fd)
        driver_failure_atom(me->port, "made_a_bad_assumption");
    return 1 == rv && !out.data;
}


static void rescan_directory(struct instance *me)
{
    struct dirent *dirent;
    DIR *dir;
    if (!(dir = fdopendir(dup(me->fd))))
        goto fail0;

    while ((dirent = readdir(dir))) {
        if ('.' == dirent->d_name[0]) continue;
        int newfd = openat(me->fd, dirent->d_name, event_open_flags);
        if (newfd < 0)
            goto fail1;

        if ((size_t)newfd > me->max_fds_seen) me->max_fds_seen = newfd;

        struct kevent in;
        EV_SET(&in, newfd, EVFILT_VNODE, EV_ADD|EV_CLEAR,
               NOTE_RENAME|NOTE_LINK|NOTE_ATTRIB|NOTE_EXTEND|NOTE_WRITE|NOTE_DELETE,
               0, 0);
        if (0 != kevent(me->kq, &in, 1, NULL, 0, NULL))
            goto fail1;

        /* see stop() for how we avoid leaking fds */
    }
    closedir(dir);
    return;

fail1:
    closedir(dir);
fail0:
    driver_failure_posix(me->port, errno);
}


static ErlDrvData start(ErlDrvPort port, char *command)
{
    unsigned long cooldown;
    char *path;
    if (!common_get_arguments(command, &path, &cooldown))
        return ERL_DRV_ERROR_BADARG;

    struct instance *me = driver_alloc(sizeof(*me));
    if (!me) return ERL_DRV_ERROR_GENERAL;
    *me = (struct instance){
        .port = port,
        .needs_rescan_p = true,
        .kq = kqueue()
    };

    if (me->kq < 0)
        goto fail0;

    me->fd = open(path, event_open_flags);
    if (-1 == me->fd)
        goto fail1;
    me->max_fds_seen = me->fd;

    /* We do the rest of the initialization in our timeout, so we
       don't need two different error paths for rescan_directory. */
    if (0 != driver_set_timer(me->port, 10))
        goto fail2;

    return (ErlDrvData)me;

fail2:
    close(me->fd);
fail1:
    close(me->kq);
fail0:
    driver_free(me);
    return ERL_DRV_ERROR_GENERAL;
}


static void cleanup_fds(struct instance *me)
{
    for (size_t i = 0; i < me->max_fds_seen; ++i) {
        if (i == (size_t)me->fd) continue;

        if (is_watched(me, i))
            close(i);
    }
    me->max_fds_seen = me->fd;
}


static void timeout(ErlDrvData me_)
{
    struct instance *me = (struct instance *)me_;

    me->n_cooldowns = 0;

    if (me->needs_rescan_p) {
        cleanup_fds(me);

        struct kevent in;
        EV_SET(&in, me->fd, EVFILT_VNODE, EV_ADD|EV_CLEAR,
               NOTE_RENAME|NOTE_LINK|NOTE_ATTRIB|NOTE_EXTEND|NOTE_WRITE|NOTE_DELETE,
               0, 0);
        if (0 != kevent(me->kq, &in, 1, NULL, 0, NULL))
            driver_failure_posix(me->port, errno);
        rescan_directory(me);
        me->needs_rescan_p = false;
    }

    /* Note that we send this changed message immediately upon
       initialization, because it simplifies our logic and it doesn't
       hurt anything.  If changes become costly, you'll probably want
       to restore the tristate logic that was here. */
    ErlDrvTermData d[] = {
        ERL_DRV_PORT, driver_mk_port(me->port),
        ERL_DRV_ATOM, driver_mk_atom("ok"),
        ERL_DRV_TUPLE, 2
    };
    erl_drv_output_term(driver_mk_port(me->port), d, sizeof(d)/sizeof(*d));

    if (driver_select(me->port, (ErlDrvEvent)(intptr_t)me->kq,
                      ERL_DRV_READ|ERL_DRV_USE, 1))
        driver_failure_atom(me->port, "driver_select_failed");
}


static void bump_timer(struct instance *me)
{
    if (++me->n_cooldowns < MAX_COOLDOWNS)
        driver_set_timer(me->port, me->cooldown);
}


static void ready_input(ErlDrvData me_, ErlDrvEvent UNUSED)
{
    struct instance *me = (struct instance *)me_;
    struct timespec ts = {0};

    struct kevent out;
    int rv;
    while (1 == (rv = kevent(me->kq, NULL, 0, &out, 1, &ts))) {
        if (!(out.flags & EVFILT_VNODE))
            continue;

        bump_timer(me);

        if (out.ident == (uintptr_t)me->fd)
            me->needs_rescan_p = true;

        if (out.fflags & NOTE_DELETE)
            close(out.ident);
    }

    if (rv < 0) {
        driver_failure_posix(me->port, errno);
        return;
    }
}


static void stop(ErlDrvData me_)
{
    struct instance *me = (struct instance *)me_;
    cleanup_fds(me);
    driver_select(me->port, (ErlDrvEvent)(intptr_t)me->kq, ERL_DRV_USE, 0);
    driver_free(me);
}


static void stop_select(ErlDrvEvent event, void *UNUSED)
{
    close((intptr_t)event);
}


static ErlDrvEntry driver_entry = {
    .init = NULL,
    .start = start,
    .stop = stop,
    .ready_input = ready_input,
    .stop_select = stop_select,
    .timeout = timeout,
    .driver_name = "dirwatch",
    .extended_marker = ERL_DRV_EXTENDED_MARKER,
    .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
    .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION
};

DRIVER_INIT(dirwatch) { return &driver_entry; }
