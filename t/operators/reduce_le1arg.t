use v6-alpha;

use Test;
plan 46;

# L<S06/"Reduction operators">

is ([**] ()), 1, "[**] () eq 1 (arguably nonsensical)";
is ([*] ()), 1, "[*] () eq 1";
dies_ok( { [/] () }, "[/] () should fail");
dies_ok( { [%] () }, "[%] () should fail");
dies_ok( { [x] () }, "[x] () should fail");
dies_ok( { [xx] () }, "[xx] () should fail");
is ([+&] ()), +^0, "[+&] () eq +^0";
dies_ok( { [+<] () }, "[+<] () should fail");
dies_ok( { [+>] () }, "[+>] () should fail");
dies_ok( { [~&] () }, "[~&] () should fail");
dies_ok( { [~<] () }, "[~<] () should fail");
dies_ok( { [~>] () }, "[~>] () should fail");
is ([+] ()), 0, "[+] () eq 0";
is ([-] ()), 0, "[-] () eq 0";
is ([~] ()), '', "[~] () eq ''";
is ([+|] ()), 0, "[+|] () eq 0";
is ([+^] ()), 0, "[+^] () eq 0";
is ([~|] ()), '', "[~|] () eq ''";
is ([~^] ()), '', "[~^] () eq ''";
# eval_is "[&] ()", all(), "[&] () eq all()";
# eval_is "[|] ()", any(), "[|] () eq any()";
# eval_is "[^] ()", one(), "[^] () eq one()";
eval_is "[!==] ()", Bool::False, "[!==] () eq False";
is ([==] ()), Bool::True, "[==] () eq True";
is ([<] ()), Bool::True, "[<] () eq True";
is ([<=] ()), Bool::True, "[<=] () eq True";
is ([>] ()), Bool::True, "[>] () eq True";
is ([>=] ()), Bool::True, "[>=] () eq True";
is ([~~] ()), Bool::True, "[~~] () eq True";
eval_is "[!~~] ()", Bool::False, "[!~~] () eq False";
is ([eq] ()), Bool::True, "[eq] () eq True)";
eval_is "[!eq] ()", Bool::False, "[!eq] () eq False";
is ([lt] ()), Bool::True, "[lt] () eq True";
is ([le] ()), Bool::True, "[le] () eq True";
is ([gt] ()), Bool::True, "[gt] () eq True";
is ([ge] ()), Bool::True, "[ge] () eq True";
is ([=:=] ()), Bool::True, "[=:=] () eq True";
eval_is "[!=:=] ()", Bool::False, "[!=:=] () eq False";
is ([===] ()), Bool::True, "[===] () eq True";
eval_is "[!===] ()", Bool::False, "[!===] () eq False";
eval_is "[eqv] ()", Bool::True, "[eqv] () eq True";
eval_is "[!eqv] ()", Bool::False, "[!eqv] () eq False";
is ([&&] ()), Bool::True, "[&&] () eq True";
is ([||] ()), Bool::False, "[||] () eq False";
is ([^^] ()), Bool::False, "[^^] () eq False";
is (defined ([//] ())), Bool::False, "[//] () not defined";
is (defined ([=] ())), Bool::False, "[=] () not defined";
is ([,] ()), (), "[,] () eq ()";
eval_is "[¥] ()", [], "[¥] () eq []";

# need to add one elems list cases
