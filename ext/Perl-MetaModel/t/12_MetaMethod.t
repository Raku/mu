#!/usr/bin/pugs

use v6;
use Test;

plan 9;

use_ok('Perl::MetaMethod');

my $method = Perl::MetaMethod::new(sub { return "Hello Meta-World" });
is($method.methodInvoke(), 'Hello Meta-World', '... got the expected value from our method');
is($method.methodVisibility(), 'public', '... by default it is public');

{
    my @params = $method.methodParams();
    is(+@params, 0, '... we dont have any params');
}

{
    my @params = $method.methodParams(1, 2, 3);
    is(+@params, 3, '... we have 3 params');
    is(~@params, '1 2 3', '... get have the right params');
}

my $method2 = Perl::MetaMethod::new(sub { return "Hello (Private) Meta-World" }, :visibility<private>);
is($method2.methodVisibility(), 'private', '... this method is private');

dies_ok {
    $method2.methodVisibility('invisible');
}, '...  method must be either public or private';
like($!, rx:perl5/^Visibility must be either \'private\' or \'public\'/, '... got the right error');