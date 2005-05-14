#!/usr/bin/pugs

use v6;
use Test;

plan 11;

=pod

Very basic instance method tests from L<S12/"Methods">

=cut

# L<S12/"Methods" /"either the dot notation or indirect object notation:">
class Foo {
  method doit ($a, $b, $c) { $a + $b + $c }
  method noargs () { 42 }
}

my $foo = Foo.new();
is($foo.doit(1,2,3), 6, "dot method invocation");

my $val;
lives_ok{
    $val = doit $foo: 1,2,3;
}, '... indirect method invocation works';
is($val, 6, '... got the right value for indirect method invocation', :todo<feature>);

is($foo.noargs, 42, "... no parentheses after method");
is($foo.noargs(), 42, "... parentheses after method");

{
    my $val;
    lives_ok {
        eval '$val = $foo.noargs()';
        die $! if $!;
    }, "... <space> + parentheses after method";
    is($val, 42, '... we got the value correctly');
}

{
    my $val;
    lives_ok {
        #eval '$val = $foo.noargs.()';
        #die $! if $!;
        die 'cannot parse "val = $foo.noargs.()"'
    }, "... '.' + parentheses after method", :todo<hardfail>;
    is($val, 42, '... we got the value correctly', :todo<feature>);
}

{
    my $val;
    lives_ok {
        #eval '$val = $foo.noargs .()';
        #die $! if $!;
        die 'cannot parse "$foo.noargs .()"'
    }, "... <space> + '.' + parentheses after method", :todo<hardfail>;
    is($val, 42, '... we got the value correctly', :todo<feature>);
}