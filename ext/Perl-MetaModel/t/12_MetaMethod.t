#!/usr/bin/pugs

use v6;
use Test;

plan 14;

use_ok('Perl::MetaMethod');
use_ok('Perl::MetaClass');

my $method = Perl::MetaMethod::new(sub { return "Hello Meta-World" });
is($method.methodClassAssociatedWith(), undef, '... our method is not associated with a class by default');
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

$! = undef;
dies_ok {
    $method2.methodVisibility('invisible');
}, '...  method must be either public or private';
like($!, rx:perl5/^Visibility must be either \'private\' or \'public\'/, '... got the right error');

my $class = Perl::MetaClass::new('Role');
$method.methodClassAssociatedWith($class);
is($method.methodClassAssociatedWith(), $class, '... our method is now associated with a class');

$! = undef;
dies_ok {
    $method.methodClassAssociatedWith(Perl::MetaClass::new('Role'))        
}, '... method can only be associated with one class';
like($!, rx:perl5/^This method has already be associated with a class/, '... got the right error');