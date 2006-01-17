#!/usr/bin/pugs

use v6;
use Test;

# test that packages work.  Note that the correspondance between type
# objects as obtained via the ::() syntax and packages is only hinted
# at in L<S10/"Packages" /or use the sigil-like/>
plan 22;

# 4 different ways to be imported
{
    package Test1;
    sub ns  { "Test1" }
    sub pkg { $?PACKAGE }
    sub test1_export is export { "export yourself!" }
    package Test2 { sub ns { "Test2" } sub pkg { $?PACKAGE } }
    package Test3; sub pkg { $?PACKAGE }
}

use t::packages::Test;

# test that all the functions are in the right place

# sanity test
is($?PACKAGE, "main", 'no declarations broke main $?PACKAGE');

# block level
is(Test1::ns, "Test1", "block-level package declarations");
is(Test1::pkg, "Test1", 'block-level $?PACKAGE var');
ok((Test1::pkg() === ::Test1), '$?PACKAGE is a type object', :todo<bug>);
dies_ok { test1_export() }, "export was not imported implicitly";

# declared packages
is(Test2::ns, "Test2", "declared package");
is(Test2::pkg, "Test2", 'declared package $?PACKAGE');

# string eval'ed packages
is(Test3::pkg, "Test3", 'eval\'ed package $?PACKAGE');
ok(Test3::pkg() === ::Test3, 'eval\'ed package type object', :todo<bug>);

# this one came from t/packages/Test.pm
is(t::packages::Test::ns, "t::packages::Test", "loaded package");
ok(t::packages::Test::pkg() === ::t::packages::Test, 'loaded package $?PACKAGE object',
   :todo<bug>);
my $x;
lives_ok { $x = test_export() }, "export was imported successfully";
is($x, "party island", "exported OK");

# exports
dies_ok { ns() }, "no ns() leaked";

# now the lexical / file level packages...
my $pkg;
dies_ok  { $pkg = Our::Package::pkg },
    "Can't see `our' packages out of scope", :todo<feature>;
lives_ok { $pkg = t::packages::Test::get_our_pkg },
    "Package in scope can see `our' package declarations";
is($pkg, "Our::Package", 'correct $?PACKAGE');
ok(!($pkg === ::Our::Package),
   'not the same as global type object');

# oh no, how do we get to that object, then?
# perhaps %t::packages::Test::<Our::Package> ?

dies_ok { $pkg = t::packages::Test::cant_see_pkg() },
    "can't see package declared out of scope", :todo<feature>;
lives_ok { $pkg = t::packages::Test::my_pkg() },
    "can see package declared in same scope";
is($pkg, "My::Package", 'correct $?PACKAGE');
ok(!($pkg === ::My::Package), 'not the same as global type object');
