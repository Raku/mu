#!/usr/bin/pugs

use v6;
use Test;

plan(9);

unless eval 'eval_perl5("1")' {
    skip_rest;
    exit;
}


eval_perl5(q/
#line 16 method.t
package FooBar;
our $VERSION = '6.0';

sub new {
    bless {}, __PACKAGE__;
}

sub foo {
    return 'foo';
}

sub echo {
    my ($self, $what) = @_;
#print "==> echo got $what\n";
    return $what;
}

sub callcode {
    my ($self, $code) = @_;
#print "==> callcode got $code\n";
    return $code->($self);
}

sub asub {
    return sub { return "asub" };
}

# takes an object and invoke me on that
sub invoke {
    my ($class, $obj) = @_;
    $obj->me ('invoking');
}

/);

{
    my $r = eval_perl5("FooBar->VERSION");
    is($r, '6.0', "class method");
}

my $obj;

{
    $obj = eval_perl5("FooBar->new");
    isa_ok($obj, 'FooBar', "blessed");
    like($obj, rx:perl5/FooBar/, "blessed");
}

{
    is($obj.foo, 'foo', 'invoke method');
}

{
    my $r = $obj.echo("bar");
    is($r, 'bar', 'invoke method with pugs arg');
}

{
    my $r = $obj.asub;
    isa_ok($r, 'CODE', "returning a coderef");
    is($r.(), 'asub', 'invoking p5 coderef');
    my $rr = $obj.callcode($r);
    is($rr, 'asub', 'invoke with p5 coderef');
}

{
    my $callback = { "baz" };
    my $r = $obj.callcode($callback);
    is($r, 'baz', 'invoke method with callback');
}

{
    class Foo6 {
	method me (Class|Foo6 $class: $arg) { 'Foo6'~$arg };
    };
    my $obj6 = Foo6.new;
    $obj = eval_perl5("FooBar->new");
#   is($obj.invoke($obj6), 'Foo6invoking', 'invoke pugs method from p5');

}
