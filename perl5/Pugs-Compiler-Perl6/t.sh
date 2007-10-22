#!/bin/sh

rm `find -name *.tc`
PCR_NO_CACHE=1 prove -Ilib -I../Pugs-Compiler-Rule/lib -I../Data-Bind/lib -I../Data-Capture/lib -r t

echo "# 6 tests and 2 subtests skipped."
echo "# Failed 2/34 test scripts. 271/630 subtests failed."

