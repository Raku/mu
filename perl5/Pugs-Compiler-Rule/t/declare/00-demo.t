# Demonstrates the usage of t::Regex and also test the tester

use t::Regex tests => 45;

run_tests();

__DATA__

=== TEST 1: basic
--- regex: .*
--- match: xyzw
--- res
$()     : xyzw
$/.from : 0
$/.to   : 4



=== TEST 2:
--- token: '((.).).'
--- match
'abc'  "1a2b"
"\t\na"

--- unmatch
ab ",\t" '\'"'

--- res1
$/     : abc
$()    : abc

$0      : ab
$0.from : 0
$0.to   : 2
$($0)   : ab

$0[0]      : a
$($0[0])   : a
$0.[ 0 ]   : a
$0[0].from : 0
$0[0].to   : 1

$0[1]  :

$/.from() : 0
$/.to( )  : 3

--- res2
$/ : "1a2"
$0 : '1a'
$0[0] : '1'

--- res3
$() : "\t\na"



=== TEST 3: named subrule
--- token:  '$<z> := [.](.)'
--- match:  "abc" "\t\n"
--- res
$<z> : 'a'
$/<z> : 'a'
$/.<z> : 'a'
$/{'z'} : 'a'
$/{"z"} : 'a'
$/.{ 'z' } : 'a'

$0 : b
$/[0] : 'b'
$/.[0] : "b"
$/[1] :

--- res2
$<z>.from : 0
$<z>.to   : 1
$0.from : 1
$0.to   : 2
$() : "\t\n"
