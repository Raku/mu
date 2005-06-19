
use v6;
use Test;

# various tests derived from L<S10/Autoloading>

plan 23;

package OughtaLoad {
    sub AUTOLOAD(*@args) {
	"\&{$_}({ @args.map:{"'$_'"}.join(", ") })"
    };
}

$_ = "test";
my $x = OughtaLoad::AUTOLOAD(1,2,3);
is($x, q[&test('1', '2', '3')], "sanity");

lives_ok { $x = OughtaLoad::test(2,3,4) },
    "AUTOLOAD", :todo<feature>;

package OughtaWork {
    our $s = 0;
    sub AUTOSCALAR { my $v = "\${$_} number {++$s}"; \$v }
    our $a = 0;
    sub AUTOARRAY  { my $v = [ "auto", $_, ++$a ];   \$v }
    our $h = 0;
    sub AUTOHASH   { my $v = { auto => $_, num => ++$h }; \$v }
    our $u = 0;
    sub AUTOSUB    { my $x = "\&{$_} number {++$u}"; sub { $x } }
    our $m = 0;
    sub AUTOMETH   { my $x = "{$_}.number {++$m}";
		     sub($self:) { "{$self}.$x" } }
}

$x = OughtaWork::AUTOSCALAR;
is($x, q[$test number 1], "AUTOSCALAR sanity test");
lives_ok { $x = $OughtaWork::foo }, "AUTOSCALAR - first", :todo<feature>;
is($x, q[$foo number 2], "Returns correct var", :todo<feature>);
lives_ok { $x = $OughtaWork::foo }, "AUTOSCALAR - repeat", :todo<feature>;
is($x, q[$foo number 2], "AUTOSCALAR only called once", :todo<feature>);
lives_ok { $x = $OughtaWork::bar }, "AUTOSCALAR - second", :todo<feature>;
is($x, q[$bar number 3], "Returns correct var", :todo<feature>);

my @a = OughtaWork::AUTOARRAY;
is(@a.join(","), q[auto,test,1], "AUTOARRAY sanity test");
lives_ok { @a = @OughtaWork::foo }, "AUTOARRAY - first", :todo<feature>;
is(@a.join(","), q[auto,foo,2], "Returns correct var", :todo<feature>);
lives_ok { @a = @OughtaWork::foo }, "AUTOARRAY - repeat", :todo<feature>;
is(@a.join(","), q[auto,foo,2], "AUTOARRAY only called once", :todo<feature>);
lives_ok { @a = @OughtaWork::bar }, "AUTOARRAY - second", :todo<feature>;
is(@a.join(","), q[auto,bar,3], "Returns correct var", :todo<feature>);

my %h = OughtaWork::AUTOHASH;
is(%h.kv.join(","), q[auto,test,num,1], "AUTOHASH sanity test");
lives_ok { %h = %OughtaWork::foo }, "AUTOHASH - first", :todo<feature>;
is(%h.kv.join(","), q[auto,foo,num,2], "Returns correct var", :todo<feature>);
lives_ok { %h = %OughtaWork::foo }, "AUTOHASH - repeat", :todo<feature>;
is(%h.kv.join(","), q[auto,foo,num,2], "AUTOHASH only called once", :todo<feature>);
lives_ok { %h = %OughtaWork::bar }, "AUTOHASH - second", :todo<feature>;
is(%h.kv.join(","), q[auto,bar,num,3], "Returns correct var", :todo<feature>);

# sanity test currently failing;
#  pugs: cannot cast from VInt 1 to Pugs.AST.Internals.VCode

# seems to work interactively though;

# pugs> package OughtaWork { our $u = 0;  sub AUTOSUB    { my $x = "\&{$_} number {++$u}"; sub { $x } } }
# undef
# pugs> my $s = OughtaWork::AUTOSUB
# sub {...}
# pugs> $s
# sub {...}
# pugs> $s()
# '& number 1'

if ( 0 ) {
# further investigation required...
my $s = OughtaWork::AUTOSUB;
say "s is $s, \$! is $!";
is($s(), q[&test number 1], "AUTOSUB sanity test");
lives_ok { $s = &OughtaWork::foo }, "AUTOSUB - first", :todo<feature>;
is($x, q[&foo number 2], "Returns correct var", :todo<feature>);
lives_ok { $s = &OughtaWork::foo }, "AUTOSUB - repeat", :todo<feature>;
is($x, q[&foo number 2], "AUTOSUB only called once", :todo<feature>);
lives_ok { $s = &OughtaWork::bar }, "AUTOSUB - second", :todo<feature>;
is($x, q[&bar number 3], "Returns correct var", :todo<feature>);

# presumably AUTOMETH on packages only has any meaning to "package
# methods"
my $s = OughtaWork.AUTOMETH;
say "s is $s";
is($s(), q[OughtaWork.test number 1], "AUTOMETH sanity test");
lives_ok { $s = OughtaWork.foo }, "AUTOMETH - first", :todo<feature>;
is($x, q[OughtaWork.foo number 2], "Returns correct var", :todo<feature>);
lives_ok { $s = OughtaWork.foo }, "AUTOMETH - repeat", :todo<feature>;
is($x, q[OughtaWork.foo number 2], "AUTOMETH only called once", :todo<feature>);
lives_ok { $s = OughtaWork.bar }, "AUTOMETH - second", :todo<feature>;
is($x, q[OughtaWork.bar number 3], "Returns correct var", :todo<feature>);
}

# to be tested (please delete this text when the tests have been
# written);

# When someone tries to actually call or access an undefined object
# (which may have come from one of the routines above, or might have
# just been declared with a body of {...}), a different set of hooks
# is used to define actual behavior at the last moment:

#    AUTOSCALARDEF
#    AUTOARRAYDEF
#    AUTOHASHDEF
#    AUTOSUBDEF
#    AUTOMETHDEF

# These routines are expected to define the object, but not to call
# it, since the call is already "scheduled" from somewhere else. (The
# goto &$AUTOLOAD is implicit, in other words. But you can hijack the
# call via the call builtin, in which case the autoloader behaves just
# like a wrapper--see A6.)

