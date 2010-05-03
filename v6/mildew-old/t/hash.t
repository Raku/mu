say "1..5";
my $hash;
$hash = ::Hash.new;
$hash{'foo'} = "ok 2";
$hash{'bar'} = "ok 1";

my $scalar;
$hash{'baz'} := $scalar;
$scalar = "ok 3";
say $hash{'bar'};
say $hash{'foo'};
say $hash{'baz'};

my $keys = $hash.keys;
if $keys.elems == 3 {
    say "ok 4";
}
my $hash2 = ::Hash.new;
$hash2{'foobar'} = 7;
my $keys2 = $hash2.keys;
if $keys2[0] eq 'foobar' {
    say "ok 5";
}
