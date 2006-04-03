use Test::More no_plan;
no t::TestFilterSimple3;

ok("No semicolons needed")
ok("Indeed nothing is needed")

use t::TestFilterSimple3;

ok("Semicolons are needed now")
