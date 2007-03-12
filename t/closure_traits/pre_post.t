use v6-alpha;

use Test;
# Test of PRE and POST traits
#
# L<S04/Closure traits>
#
# I don't get this one working:
## L<S06/Properties and traits/Subroutine traits>

# TODO: Test PRE and POST with inheritance

plan 13;

my $foo = '
sub foo(Num $i) {
    PRE {
        $i < 5
    }
    return 1;
}
';

sub bar(int $i) {
    return 1;
    POST {
        $i < 5;
    }
}

ok eval($foo ~ 'foo(2);'), 'sub with PRE compiles and runs';
ok eval(bar(3)), 'sub with POST compiles';

try {
    eval($foo ~ 'foo(10)');
}

ok defined($!), 'Violated PRE fails OK';

try {
    bar(10);
}
ok defined($!), 'violated POST fails OK';

# multiple PREs und POSTs

my $baz = '
sub baz (Num $i) {
	PRE {
		$i > 0
	}
	PRE {
		$i < 23
	}
	return 1;
}
';
ok($baz ~ 'baz(2)', 'sub with two PREs compiles and runs');

eval( $baz ~ 'baz(-1)');
ok(defined($!), 'sub with two PREs fails when first is violated');

eval( $baz ~ 'baz(42)');
ok(defined($!), 'sub with two PREs fails when second is violated');

sub qox (Num $i) {
	return 1;
	POST {
		$i > 0
	}
	POST {
		$i < 42
	}
}

ok(qox(23), "sub with two POSTs compiles and runs");

try {
	qox(-1);
}

ok(defined($!), "sub with two POSTs fails if first POST is violated");

try {
	qox(123);
}

ok(defined($!), "sub with two POSTs fails if second POST is violated");

# inheritance

my $ih_pre = 
' class Foo {
    method test(Num $i) {
        PRE {
	    $i > 0
        }
		
        return 1;
    }
}

class Bar is Foo{
    method test(Num $i){
        PRE {
            $i < 23
        }
        return 1;
    }
}
my $foo = Bar.new; ';

ok(eval($ih_pre ~ '$foo.test(1)'), "PRE in methods compiles and runs");

try {
    eval($ih_pre ~ '$foo.test(-1)');
}

ok(defined($!), "violated PRE inherited from ancestor fails OK");

try {
    eval($ih_pre ~ '$foo.test(42)');
}
ok(defined($!), "violated PRE in method fails OK");
