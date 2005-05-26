#!/usr/bin/pugs

use v6;
use Test;

plan(6);

unless eval 'eval_perl5("1")' {
    skip_rest;
    exit;
}


eval_perl5(q/
package FooBar;
our $VERSION = '6.0';

sub new {
    bless {}, __PACKAGE__;
}

sub foo {
    return 'foo';
}

sub echo {
    my $self = shfit;
    return shift;
}

sub callcode {
    my ($self, $code) = @_;
    return $code->($self);
}

/);

{
    my $r = eval_perl5("FooBar->VERSION");
    is($r, '6.0', "class method");
}

my $obj;

{
    $obj = eval_perl5("FooBar->new");
    isa_ok($obj, Scalar::Perl5, "blessed");
    like($obj, rx:perl5/FooBar/, "blessed");
}

{
    is($obj.foo, 'foo', 'invoke method');
}

{
    my $r = eval '$obj.echo("bar")';
    is($r, 'bar', 'invoke method');
}

{
    my $callback = -> { "baz" };
    my $r = eval '$obj.callcode($callback)';
    is($r, 'baz', 'invoke method with callback');
}

