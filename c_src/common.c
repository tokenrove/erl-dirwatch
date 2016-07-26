#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"


bool common_get_arguments(char *command, char **path, unsigned long *cooldown)
{
    if (!command || !path || !cooldown) return false;

    command = strchr(command, ' ');
    if (!command) return false;

    command += strspn(command, " ");
    if (!*command) return false;

    *cooldown = strtoul(command, &command, 10);
    if (' ' != *command) return false;
    command += strspn(command, " ");
    if (!*command) return false;
    *path = command;
    return true;
}
