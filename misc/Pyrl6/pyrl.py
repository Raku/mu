#!/usr/bin/env python

import sys
import Pyrl

args = []
if len(sys.argv) > 1:
	args = sys.argv[1:]
else:
	args.append('-')

code = ''
if args[0] == '-':
	code = sys.stdin.read()
else:
	code = open(args[0]).read()

Pyrl.Backend(code, args).run()

# :mode=python:
