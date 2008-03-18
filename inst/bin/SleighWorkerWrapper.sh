#!/bin/sh

case $# in
0) exit 0
esac

CMD=$1
shift

# try to avoid inheriting the connection to the nws server
exec 3>&- 4>&- 5>&- 6>&- 7>&- 8>&- 9>&-
exec "$CMD" "$@"
