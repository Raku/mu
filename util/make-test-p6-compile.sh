#!/bin/sh

# This utility just runs a test to see that all the bundled Perl 6 modules 
# will compile, without executing them.  Its functionality would ideally be 
# combined into the 'make' process, and be invokable at any time that 
# 'make test' or 'make smoke' is valid.  Until then, you can invoke this 
# instead of the latter when your cwd is the pugs root.

# Run with:
#   util/make-test-p6-compile.sh

# The utility is simple, its output is meant to be looked at by humans, and 
# that output is not harnessed.
# If all the output lines say "Foo syntax OK" then there are no problems. 
# If any lines say something else, then you know what needs to be repaired 
# for everything to compile.

# The utility is modified by Darren Duncan 
# from a one-liner supplied by Audrey Tang on #perl6.

find blib6/lib -name '*.pm' -exec ./pugs -e "@*INC.unshift('blib6/lib'); say 'require \'{}\';'; require '{}';" ';'
