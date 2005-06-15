#!/bin/sh
# Sets the usual svn properties on a file.

[ "$1" ] || {
  echo "Usage: $0 file    # Sets the usual svn properties on \"file\"." >&2
  exit 1
}

svn propset svn:eol-style "native"                    "$1"
svn propset svn:mime-type "text/plain; charset=UTF-8" "$1"
