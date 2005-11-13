#!/bin/sh
# Adds text files to svn and sets the usual svn properties on them.
# This script is an extended copy of add-svn-props.sh.

# It should not be used on directories like plain 'svn add' can be, because
# it won't recursively add the svn props too.  In case someone tries that,
# the --non-recursive flag should prevent the side effects.
# Feel free to update the script so it has a recursive option if desired.

if [ -d .svn ]; then
  BINARY=svn
else
  BINARY=svk
fi

[ "$1" ] || {
  echo -e "Usage: $0 file1 file2...\n       # Adds the given files to svn as text files." >&2
  exit 1
}

for i in "$@"; do
  $BINARY add --non-recursive "$i"
  $BINARY propset svn:eol-style "native"                    "$i"
  $BINARY propset svn:mime-type "text/plain; charset=UTF-8" "$i"
done
