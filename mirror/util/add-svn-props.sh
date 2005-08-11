#!/bin/sh
# Sets the usual svn properties on files.

if [ -d .svn ]; then
	BINARY=svn
else
	BINARY=svk
fi

[ "$1" ] || {
  echo -e "Usage: $0 file1 file2...\n       # Sets the usual svn properties on the given files." >&2
  exit 1
}

for i in "$@"; do
  $BINARY propset svn:eol-style "native"                    "$i"
  $BINARY propset svn:mime-type "text/plain; charset=UTF-8" "$i"
done
