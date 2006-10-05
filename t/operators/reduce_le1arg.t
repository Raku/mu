use v6-alpha;

use Test;
plan 46;

# smartlink to top and bottom of long table
# L<S03/Reduction operators/"Builtin reduce operators return the following identity values">
# L<S03/Reduction operators/"[¥]()       # []">

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
# is eval("[&] ()"), all(), "[&] () eq all()";
# is eval("[|] ()"), any(), "[|] () eq any()";
# is eval("[^] ()"), one(), "[^] () eq one()";
is eval("[!==] ()"), Bool::False, "[!==] () eq False";
is ([==] ()), Bool::True, "[==] () eq True";
is ([<] ()), Bool::True, "[<] () eq True";
is ([<=] ()), Bool::True, "[<=] () eq True";
is ([>] ()), Bool::True, "[>] () eq True";
is ([>=] ()), Bool::True, "[>=] () eq True";
is ([~~] ()), Bool::True, "[~~] () eq True";
is eval("[!~~] ()"), Bool::False, "[!~~] () eq False";
is ([eq] ()), Bool::True, "[eq] () eq True)";
is eval("[!eq] ()"), Bool::False, "[!eq] () eq False";
is ([lt] ()), Bool::True, "[lt] () eq True";
is ([le] ()), Bool::True, "[le] () eq True";
is ([gt] ()), Bool::True, "[gt] () eq True";
is ([ge] ()), Bool::True, "[ge] () eq True";
is ([=:=] ()), Bool::True, "[=:=] () eq True";
is eval("[!=:=] ()"), Bool::False, "[!=:=] () eq False";
is ([===] ()), Bool::True, "[===] () eq True";
is eval("[!===] ()"), Bool::False, "[!===] () eq False";
is eval("[eqv] ()"), Bool::True, "[eqv] () eq True";
is eval("[!eqv] ()"), Bool::False, "[!eqv] () eq False";
is ([&&] ()), Bool::True, "[&&] () eq True";
is ([||] ()), Bool::False, "[||] () eq False";
is ([^^] ()), Bool::False, "[^^] () eq False";
is (defined ([//] ())), Bool::False, "[//] () not defined";
is (defined ([=] ())), Bool::False, "[=] () not defined";
is ([,] ()), (), "[,] () eq ()";
is eval("[¥] ()"), [], "[¥] () eq []";

# need to add one elems list cases
