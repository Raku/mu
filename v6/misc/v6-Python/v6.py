#!/usr/bin/env python

import MiniPerl.Parse
import sys

data = ''
if len(sys.argv) > 1:
	data = open(sys.argv[1]).read()
else:
	data = sys.stdin.read()

print MiniPerl.Parse.yacc.parse(data)
