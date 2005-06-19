
use v6;
use Test;

# test that packages work.  Note that the correspondance between type
# objects as obtained via the ::() syntax and packages is only hinted
# at in S10
plan 23;

# 4 different ways to be imported
{
    package Test1;
    sub ns  { "Test1" }
    sub pkg { $?PACKAGE }
    sub test1_export is export { "export yourself!" }
}
package Test2 { sub ns { "Test2" } sub pkg { $?PACKAGE } }
eval 'package Test3; sub pkg { $?PACKAGE }';
eval 'use t::packages::Test;';

# test that all the functions are in the right place

# sanity test
is($?PACKAGE, "main", 'no declarations broke main $?PACKAGE');

# block level
is(Test1::ns, "Test1", "block-level package declarations");
is(Test1::pkg, "Test1", 'block-level $?PACKAGE var');
ok(Test1::pkg =:= ::Test1, '$?PACKAGE is a type object', :todo<bug>);
my $x;
lives_ok { $x = test1_export() }, "export was imported successfully";
is($x, "export yourself!", "exported OK");

# declared packages
is(Test2::ns, "Test2", "declared package");
is(Test2::pkg, "Test2", 'declared package $?PACKAGE');

# string eval'ed packages
is(Test3::pkg, "Test3", 'eval\'ed package $?PACKAGE');
ok(Test3::pkg =:= ::Test3, 'eval\'ed package type object', :todo<bug>);

# this one came from t/packages/Test.pm
is(Our::Test::ns, "Our::Test", "loaded package");
ok(Our::Test::pkg =:= ::Our::Test, 'loaded package $?PACKAGE object',
   :todo<bug>);
lives_ok { $x = test_export() }, "export was imported successfully";
is($x, "party island", "exported OK");

# exports
dies_ok { ns() }, "no ns() leaked";

# now the lexical / file level packages...
my $pkg;
dies_ok  { $pkg = Our::Package::pkg },
    "Can't see `our' packages out of scope";
lives_ok { $pkg = Our::Test::get_our_pkg },
    "Package in scope can see `our' package declarations", :todo<feature>;
is($pkg, "Our::Package", 'correct $?PACKAGE', :todo<feature>);
ok(!($pkg =:= ::Our::Package),
   'not the same as global type object');

# oh no, how do we get to that object, then?
# perhaps %Our::Test::<Our::Package> ?

dies_ok { $pkg = Our::Test::cant_see_pkg() },
    "can't see package declared out of scope";
lives_ok { $pkg = Our::Test::my_pkg() },
    "can see package declared in same scope", :todo<feature>;
is($pkg, "My::Package", 'correct $?PACKAGE', :todo<feature>);
ok(!($pkg =:= ::My::Package), 'not the same as global type object');
