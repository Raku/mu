#!/usr/bin/pugs

use v6;
use Test;

# various tests derived from L<S10/Autoloading>

plan 38;

package OughtaLoad {
    sub AUTOLOAD(*@args) {
	"\&{$_}({ @args.map:{"'$_'"}.join(", ") })"
    };
}

# currently, $_ is passed in as a global as per spec!
$_ = "test";
my $x = OughtaLoad::AUTOLOAD(1,2,3);
is($x, q[&test('1', '2', '3')], "sanity");

lives_ok { $x = OughtaLoad::test(2,3,4) },
    "AUTOLOAD", :todo<feature>;

package OughtaWork {
    our $s = 0;
    sub AUTOSCALAR($_) { my $v = "\${$_} number {++$s}";
	             eval "our \${$_} := \$v";
                     \$v }
    our $a = 0;
    sub AUTOARRAY($_)  { my @v = ( "auto", $_, ++$a );
	             eval "our @{$_} := @v";
		     \@v }
    our $h = 0;
    sub AUTOHASH($_)   { my %v = ( auto => $_, num => ++$h );
	             eval "our %{$_} := %v";
		     \%v }
    our $u = 0;
    sub AUTOSUB($_)    { my $x = "\&{$_} number {++$u}";
		     my $sub = sub { $x };
	             eval "our &{$_} := \$sub";
		     return $sub;
 		   }
    our $m = 0;
    method AUTOMETH($_) { my $x = "$?SELF {$_}.number {++$m}";
		     my $method = sub($self:) { "{$self}.$x" };
		     eval "our &{$_} := \$method";
		     return $method;
		   }
}

# however, the use of $_ in the above is because the first argument is
# bound to $_ via "default invocants" (which seems to not be
# working... hmm)
$_ = "poison";

# scalar
$x = OughtaWork::AUTOSCALAR("test");
is($x, q[$test number 1], "AUTOSCALAR sanity test");

lives_ok { $x = $OughtaWork::foo }, "AUTOSCALAR - first";
is($x, q[$foo number 2], "Returns correct var", :todo<feature>);

$x="";
lives_ok { $x = $OughtaWork::foo }, "AUTOSCALAR - repeat";
is($x, q[$foo number 2], "AUTOSCALAR only called once", :todo<feature>);

lives_ok { $x = $OughtaWork::bar }, "AUTOSCALAR - second";
is($x, q[$bar number 3], "Returns correct var", :todo<feature>);


# array
my $a = OughtaWork::AUTOARRAY("test");
is($a.join(","), q[auto,test,1], "AUTOARRAY sanity test");

my @a;
lives_ok { @a = @OughtaWork::foo }, "AUTOARRAY - first";
is(@a.join(","), q[auto,foo,2], "Returns correct var", :todo<feature>);

@a=();
lives_ok { @a = @OughtaWork::foo }, "AUTOARRAY - repeat";
is(@a.join(","), q[auto,foo,2], "AUTOARRAY only called once", :todo<feature>);

lives_ok { @a = @OughtaWork::bar }, "AUTOARRAY - second";
is(@a.join(","), q[auto,bar,3], "Returns correct var", :todo<feature>);


# hash
my %h = OughtaWork::AUTOHASH("test");
is(%h.kv.join(","), q[auto,test,num,1], "AUTOHASH sanity test");

lives_ok { %h = %OughtaWork::foo }, "AUTOHASH - first";
is(%h.kv.join(","), q[auto,foo,num,2], "Returns correct var", :todo<feature>);

%h=();
lives_ok { %h = %OughtaWork::foo }, "AUTOHASH - repeat";
is(%h.kv.join(","), q[auto,foo,num,2], "AUTOHASH only called once", :todo<feature>);

lives_ok { %h = %OughtaWork::bar }, "AUTOHASH - second";
is(%h.kv.join(","), q[auto,bar,num,3], "Returns correct var", :todo<feature>);


# sub
my $s = OughtaWork::AUTOSUB("test");
my $v = $s();
is($v, q[&test number 1], "AUTOSUB sanity test");

$v="";
eval_ok q{ $s = &OughtaWork::foo; $v = $s(); },
	"AUTOSUB - first", :todo<feature>;
is($v, q[&foo number 2], "Returns correct var", :todo<feature>);

$v="";
eval_ok q{ $s = &OughtaWork::foo; $v = $s();  },
	"AUTOSUB - repeat", :todo<feature>;
is($v, q[&foo number 2], "AUTOSUB only called once", :todo<feature>);

$v="";
eval_ok q{ $s = &OughtaWork::bar; $v = $s();  },
	"AUTOSUB - second", :todo<feature>;
is($v, q[&bar number 3], "Returns correct var", :todo<feature>);


# presumably AUTOMETH on packages only has any meaning to "package
# methods"; they have to be classes or roles for AUTOMETH to be method
# lookups.
my $inv = ::OughtaWork;
eval_ok q{ $s = OughtaWork.AUTOMETH("test"); $v = $s($inv:) },
	"AUTOMETH - sanity", :todo<bug>;
is($v, q[OughtaWork.test number 1], "AUTOMETH sanity test", :todo<bug>);

$v = "";
eval_ok q{ $s = OughtaWork.foo; $v = $s($inv:) },
	"AUTOMETH - first", :todo<feature>;
is($x, q[OughtaWork.foo number 2], "Returns correct var", :todo<feature>);

$s = sub { };
eval_ok q{ $s = OughtaWork.foo; $v = $s($inv:)  },
	"AUTOMETH - repeat", :todo<feature>;
is($x, q[OughtaWork.foo number 2], "AUTOMETH only called once", :todo<feature>);
eval_ok q{ $s = OughtaWork.bar; $v = $s($inv:)  },
	"AUTOMETH - second", :todo<feature>;
is($x, q[OughtaWork.bar number 3], "Returns correct var", :todo<feature>);

# discovered bugs (TODO: write tests to track down:)

# 1. changing the is($v, ...) to is($s(), ...) causes:

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

# further investigation required...

# 2. named subs don't seem to get implicit arguments via $_ (actually
#    as per S04 this seems to be design behaviour)

# 3. Various nonsense with using a Package as an invocant in a method
#    call

# 4. Couldn't take ref of a method via &Package.foo

