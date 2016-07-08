#!/usr/bin/env bash
#
# Evaluate which function is fastest on this machine right now, and
# build it into a shared object.  This is just a shell script rather
# than a Makefile since there's not much point avoiding rebuilds here.

set -eu -o pipefail

ERTS_INCLUDE_DIR=${ERTS_INCLUDE_DIR:-$(erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")}
ERL_INCLUDE_DIR=${ERL_INCLUDE_DIR:-$(erl -noshell -s init stop -eval "io:format(\"~s/usr/include/\", [code:root_dir()]).")}
CC=${CC:-cc}
DEFAULT_CFLAGS="-O3 -march=native -mtune=native -ggdb -Wall -Wextra -Wno-missing-field-initializers"
CFLAGS="-fPIC -I${ERTS_INCLUDE_DIR} -I${ERL_INCLUDE_DIR} -std=gnu11 ${CFLAGS:-$DEFAULT_CFLAGS}"
LDFLAGS=${LDFLAGS:-}

OS="$(uname -s)"

case "$OS" in
    Linux)
        IMPLEMENTATION=dirwatch_inotify
        ;;
    Darwin)
        LDFLAGS="$LDFLAGS -flat_namespace -undefined suppress"
        echo "Not implemented yet for $OS.  Sorry."
        exit 1
        ;;
    *)
        echo "Not implemented yet for $OS.  Sorry."
        exit 1
        ;;
esac

TARGET=${TARGET:-./priv/dirwatch.so}
SRC=./c_src

mkdir -p priv

up_to_date_p() {
    for i in $SRC/*.c; do
        if [ "$TARGET" -ot "$i" ]; then exit 1; fi
    done
    exit 0
}

if (up_to_date_p); then exit 0; fi

exec "$CC" $CFLAGS -shared -o "$TARGET" $SRC/$IMPLEMENTATION.c $LDFLAGS
