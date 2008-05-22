use v6;

use Test;

plan 4;

if $?PUGS_BACKEND eq "BACKEND_JAVASCRIPT" {
  skip_rest "YAML support not available in PIL2JS";
  exit;
}

ok(eval(q{#eval("- *a\n a: b\n- *a\n b: c\n",:lang<yaml>)}),'yaml parsing can kill pugs all the way dead');

ok(undef, "Bug workaround for release.", :todo<bug>);
# Bug workaround: the order of the next two tests determines
# whether the second one ('test') fails under smoke.
is( eval('test', :lang<yaml>), 'test', '"test" roundtrips' );
ok( !defined(eval(undef, :lang<yaml>)), '"undef" roundtrips' );

