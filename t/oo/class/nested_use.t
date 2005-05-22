#!/usr/bin/pugs

use v6;
use Test;

plan 9;

BEGIN {
   @*INC.unshift('t/oo/class/TestFiles');
}

lives_ok {
    require Foo;
}, '... we can require Foo';

lives_ok {
    require Bar;
}, '... we can require Bar (which requires Foo)';

lives_ok {
    require FooBar;
}, '... we can require FooBar (which requires Bar (which requires Foo))';

my $foobar = ::FooBar.new();

{
    my $val;
    lives_ok {
        $val = $foobar.foobar()
    }, '... the FooBar::foobar method resolved';
    is($val, 'foobar', '... the FooBar::foobar method resolved');
}

{
    my $val;
    lives_ok {
        $val = $foobar.bar()
    }, '... the Bar::bar method resolved', :todo<bug>;
    is($val, 'bar', '... the Bar::bar method resolved', :todo<bug>);
}

{
    my $val;
    lives_ok {
        $val = $foobar.foo()
    }, '... the Foo::foo method resolved', :todo<bug>;
    is($val, 'foo', '... the Foo::foo method resolved', :todo<bug>);
}
