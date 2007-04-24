#!/usr/bin/env python

import MiniPerl.Parse as parse
import sys

if len(sys.argv) > 1:
	data = open(sys.argv[1]).read()
else:
	data = sys.stdin.read()

print parse.yacc.parse(data)
