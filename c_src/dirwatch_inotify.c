/* An Erlang driver for (a minimal subset of) inotify
 * Part of erl-dirwatch
 */


#include <stdint.h>
#include <unistd.h>

#include <sys/inotify.h>

#include "ei.h"
#include "erl_driver.h"

#include "common.h"
#include "macrology.h"


struct instance {
    ErlDrvPort port;
    int fd;
    int wd;
    unsigned n_cooldowns;
    unsigned long cooldown;
};

/* Arbitrary; number of times we can reset the cooldown counter
   without allowing it to timeout. */
enum { MAX_COOLDOWNS = 12 };


static ErlDrvData start(ErlDrvPort port, char *command)
{
    unsigned long cooldown;
    char *path;
    if (!common_get_arguments(command, &path, &cooldown))
        return ERL_DRV_ERROR_BADARG;

    struct instance *me = driver_alloc(sizeof(*me));
    if (!me) return ERL_DRV_ERROR_GENERAL;
    *me = (struct instance){0};
    me->cooldown = cooldown;
    me->port = port;

    me->fd = inotify_init1(IN_NONBLOCK | IN_CLOEXEC);
    if (me->fd < 0)
        goto fail0;

    int wd = inotify_add_watch(me->fd, path,
                               IN_CREATE|IN_DELETE|IN_MOVE|IN_CLOSE_WRITE);
    if (-1 == wd)
        goto fail1;

    if (driver_select(me->port, (ErlDrvEvent)(intptr_t)me->fd,
                      ERL_DRV_READ|ERL_DRV_USE, 1))
        goto fail1;

    return (ErlDrvData)me;

fail1:
    close(me->fd);
fail0:
    driver_free(me);
    return ERL_DRV_ERROR_GENERAL;
}


static void stop(ErlDrvData me_)
{
    struct instance *me = (struct instance *)me_;
    driver_select(me->port, (ErlDrvEvent)(intptr_t)me->fd, ERL_DRV_USE, 0);
    driver_free(me);
}


static void ready_input(ErlDrvData me_, ErlDrvEvent event)
{
    struct instance *me = (struct instance *)me_;

    int fd = (intptr_t)event;
    /* Throw away inotify events until we can't read more. */
    char buf[4096];
    ssize_t len;
    while ((len = read(fd, buf, sizeof(buf))) > 0);

    if (++me->n_cooldowns < MAX_COOLDOWNS)
        driver_set_timer(me->port, me->cooldown);
}


static void timeout(ErlDrvData me_)
{
    struct instance *me = (struct instance *)me_;

    me->n_cooldowns = 0;
    ErlDrvTermData d[] = {
        ERL_DRV_PORT, driver_mk_port(me->port),
        ERL_DRV_ATOM, driver_mk_atom("ok"),
        ERL_DRV_TUPLE, 2
    };
    erl_drv_output_term(driver_mk_port(me->port), d, sizeof(d)/sizeof(*d));
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
