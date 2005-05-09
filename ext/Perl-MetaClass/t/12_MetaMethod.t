#!/usr/bin/pugs

use v6;
use Test;

use_ok('Perl::MetaMethod');

my $method = Perl::MetaMethod::new(sub { return "Hello Meta-World" });
is($method.methodInvoke(), 'Hello Meta-World', '... got the expected value from our method');

{
    my @params = $method.methodParams();
    is(+@params, 0, '... we dont have any params');
}

{
    my @params = $method.methodParams(1, 2, 3);
    is(+@params, 3, '... we have 3 params');
    is(~@params, '1 2 3', '... get have the right params');
}
